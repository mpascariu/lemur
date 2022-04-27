import os
import psycopg2
import datetime
import json
from ast import literal_eval


def timestr():
    return (
        datetime.datetime.utcnow()
        .replace(tzinfo=datetime.timezone.utc, microsecond=0)
        .isoformat(sep=" ")[:-3]
    )


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

    integer_args = []
    json_args = []
    boolean_args = []
    date_args = []
    quote_args = [] + json_args + date_args

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
                int(float(args.get(i)))
            except:
                status = 400
                message = "Bad Request: '{}' cannot be coerced to an integer.".format(i)
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

        # quote strings
        for i in set(args).intersection(quote_args):
            args[i] = "'" + str(args[i].replace("'", '"')) + "'"

    return {"status": status, "message": message, "args": args}


def validate_token(token, access="read"):
    conn = psycopg2.connect(
        host=os.environ.get("POSTGRES_HOST"),
        database="gbd2019",
        user=os.environ.get("POSTGRES_USER"),
        password=os.environ.get("POSTGRES_PASSWORD"),
    )
    cur = conn.cursor()
    sql_query = (
        "SELECT id, " + access + " FROM users WHERE token='{}';".format(token)
    )
    cur.execute(sql_query)
    conn.commit()
    response = cur.fetchall()

    if len(response) > 0:
        contributor_id = str(response[0][0])
        authenticated = response[0][1]
        cur.execute(
            "UPDATE contributors SET reqs_total=reqs_total+1, reqs_today=reqs_today+1 WHERE id="
            + contributor_id
            + ";"
        )
        conn.commit()
    else:
        contributor_id = None
        authenticated = False

    if authenticated:
        status = 200
        message = "OK: Token successfully authenticated for " + access + " access."
    else:
        status = 401
        message = "Unauthorized: Token failed authentication for " + access + " access."

    conn.close()
    return {"status": status, "message": message, "user_id": contributor_id}


