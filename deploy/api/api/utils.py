import os

import psycopg
import datetime
import pandas as pd
from ast import literal_eval

# Database connection settings come from environment variables so that the
# credentials never live in the source code. Production supplies them via the
# deployment platform (docker-compose .env / cloud environment variables);
# the defaults match the local docker-compose stack. See .env.example at the
# repo root.
DB_HOST = os.environ.get("LEMUR_DB_HOST", "postgres")
DB_NAME = os.environ.get("LEMUR_DB_NAME", "gbd_lemur_db")
DB_USER = os.environ.get("LEMUR_DB_USER", "lemur")
DB_PASSWORD = os.environ.get("LEMUR_DB_PASSWORD", "")

if not DB_PASSWORD:
    raise RuntimeError(
        "LEMUR_DB_PASSWORD is not set; refusing to start without a database "
        "password. Set it in the deployment environment (see .env.example)."
    )


def db_connect():
    """Open a connection to the lemur database."""
    return psycopg.connect(
        dbname=DB_NAME,
        host=DB_HOST,
        user=DB_USER,
        password=DB_PASSWORD,
    )

def timestr():
    return (
        datetime.datetime.now(datetime.timezone.utc)
        .replace(microsecond=0)
        .isoformat(sep=" ")[:-3]
    )


def query(sql, params=None):
    """Run a SELECT and wrap the result in the API's standard envelope.

    `sql` must be a literal defined in this codebase; every value derived
    from a request goes in `params` as a bind parameter. Besides preventing
    injection, passing parameters makes psycopg use the extended query
    protocol, which rejects multiple statements in one execute() -- so a
    stray ';' cannot append a second statement.

    Executed directly through psycopg (no pandas.read_sql: that path needs
    SQLAlchemy on modern pandas). Column order and dtypes are preserved, and
    df.to_json() keeps the response format the API consumers expect.
    """
    status = 200
    message = ""
    data = "{}"

    try:
        # Context managers so the connection is released on the error path
        # too; the previous version leaked one per failed query.
        with db_connect() as conn:
            with conn.cursor() as cur:
                cur.execute(sql, params)
                cols = [d.name for d in cur.description]
                rows = cur.fetchall()
        df = pd.DataFrame(rows, columns=cols)
    except Exception:
        status = 500
        message = "Internal Server Error: Error returned from PostgreSQL server on SELECT."
        return {
            "status": status,
            "message": message,
            "timestamp": timestr(),
            "data": data
        }

    if df.shape[0] == 0:
        message = "OK: No data match this query."
    else:
        message = "OK: Data successfully selected from database."

    return {
        "status": status,
        "message": message,
        "timestamp": timestr(),
        "data": df.to_json()
    }


def check_args(args, required=[], required_oneof=[], optional=[]):
    """Check arguments of GET request
    Args:
        args (dict): Arguments of GET request
        required (list): Names of required arguments
        required_oneof (list): Names of required arguments for which at least one is required
        optional (list): Names of optional arguments
    Returns:
        dict: http response compatible with json format along with modified args object
    """

    # argument lists
    # unlisted arguments: token
    required_globally = []  # 'valid'

    integer_args = ['age', 'year']
    json_args = []
    boolean_args = []
    date_args = []
    list_args = ['region']
    quote_args = ['sex'] + json_args + date_args

    sex_allowed = ['both', 'male', 'female']
    age_allowed = [0, 1, 2, *range(5, 100, 5)]
    year_allowed = [*range(1990, 2016, 5), 2019, 2020, 2021, 2023]

    # initialize response
    status = 200
    message = ""

    # remove unused arguments
    args = {
        key: value
        for key, value in args.items()
        if key in required + required_oneof + optional
    }

    # run checks
    for i in required_globally:
        if not i in required:
            required.append(i)
    if not all(i in args for i in required):
        status = 400
        message = "Bad Request: All of these arguments are required {}.".format(
            required
        )

    elif len(required_oneof) > 0 and not any(i in args for i in required_oneof):
        status = 400
        message = (
            "Bad Request: At least one of these arguments are required {}.".format(
                required_oneof
            )
        )

    elif not all(
        isinstance(args.get(i), int) for i in set(args).intersection(integer_args)
    ):
        for i in set(args).intersection(integer_args):
            try:
                # Store the coerced value: it is passed to the database as a
                # bind parameter, so it must be an int rather than the
                # original string.
                args[i] = int(float(args.get(i)))
            except:
                status = 400
                message = "Bad Request: '{}' cannot be coerced to an integer.".format(i)
                break

    elif not all(
        isinstance(args.get(i), list) for i in set(args).intersection(list_args)
    ):
        for i in set(args).intersection(list_args):
            try:
                # literal_eval evaluates literals only (no code execution),
                # but cap the input first so a deeply nested value cannot be
                # used to burn CPU.
                if len(str(args.get(i))) > 500:
                    raise ValueError("argument too long")
                args[i] = literal_eval(args.get(i))
                if not isinstance(args[i], list):
                    args[i] = [args[i]]
                if not isinstance(args[i], list):
                    raise Exception
            except:
                status = 400
                message = "Bad Request: '{}' cannot be coerced to a list. Try {}=['{}'].".format(i, i, args[i])
                break

    elif not all(
        isinstance(args.get(i), datetime.date)
        for i in set(args).intersection(date_args)
    ):
        for i in set(args).intersection(date_args):
            try:
                datetime.datetime.strptime(args.get(i), "%Y-%m-%d")
            except:
                status = 400
                message = "Bad Request: '{}' cannot be coerced to a date of format YYYY-MM-DD.".format(
                    i
                )
                break

    if status == 200:

        # strings to boolean
        for i in boolean_args:
            if i in args and not isinstance(args.get(i), bool):
                args[i] = str(args.get(i)).lower() in [
                    "true",
                    "t",
                    "yes",
                    "y",
                    "on",
                    "1",
                ]

        # String values are NOT quoted here. They are passed to the
        # database as bind parameters (see endpoints.api_fun), so wrapping
        # them in quotes would make the quotes part of the value.

    return {"status": status, "message": message, "args": args}


def validate(ip):

    rpm = 30
    daily_limit = rpm * 60 * 24

    authenticated = False

    with db_connect() as conn:
        with conn.cursor() as cur:
            # Read the stored counter, not the row count. api_requests has
            # UNIQUE(date, ip), so count(*) was only ever 0 or 1 -- which made
            # the "response < daily_limit" branch below always true and the
            # daily limit unenforceable.
            cur.execute(
                "select requests from api_requests "
                "where ip = %s and date = current_date;",
                (ip,),
            )
            row = cur.fetchone()

            if row is None:
                authenticated = True
                cur.execute(
                    "insert into api_requests (date, ip) values (current_date, %s);",
                    (ip,),
                )
            elif row[0] < daily_limit:
                authenticated = True
                cur.execute(
                    "update api_requests set requests = requests + 1 "
                    "where ip = %s and date = current_date;",
                    (ip,),
                )

    if authenticated:
        status = 200
        message = "OK: Successfully authenticated."
    else:
        status = 401
        message = "Unauthorized: Daily limit exceeded ({} API requests).".format(daily_limit)

    return {"status": status, "message": message}


