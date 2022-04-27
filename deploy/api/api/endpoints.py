#!/usr/bin/python3

import os
import datetime
import psycopg2
import pandas as pd
from api.utils import timestr, check_args, validate_token



# query cause_of_death endpoint
def cod_fun(args):
    """Process requests to API endpoint '/cause_of_death' by selecting queried data from a PostgreSQL table.
    Args:
        args (dict): Arguments of GET request passed from request.args
    Returns:
        dict: http response compatible with json format
    """
    time_start = datetime.datetime.now()

    status = 200
    data = None

    # # check arguments
    # result = check_args(
    #     args,
    #     required=[],
    #     required_oneof=[],
    #     optional=[],
    # )
    # args = result.get("args")
    # status = result.get("status")
    # message = result.get("message")
    # data = None
    #
    # if status == 200:
    #
    #     # validate token
    #     result = validate_token(token=args.get("token"), access="read")
    #
    #     status = result.get("status")
    #     if status == 200:
    #         # args['user_id'] = result.get('user_id')
    #         args.pop("token")
    #     else:
    #         message = result.get("message")

    if status == 200:

        # list query columns
        cols = []

        # create sql query
        sql_query = f"SELECT {', '.join(cols)}\nFROM {args['table']}\nWHERE "

        where_clauses = []
        for col in [
            "contributor_id",
            "country",
            "gender",
            "age_min",
            "age_max",
        ]:
            if col in args:
                where_clauses.append(f"{col} = {args[col]}")
        if "date_start" in args:
            where_clauses.append(f"timestamp_iso::date >= {args['date_start']}")
        if "date_end" in args:
            where_clauses.append(f"timestamp_iso::date <= {args['date_end']}")
        for col in ["home", "recent", "travel_in"]:
            if col in args:
                where_clauses.append(
                    f"(geo_locations #>'{{location_types}}' ? '{col}') = {str(args[col]).lower()}"
                )
        if "geo_scale" in args:
            where_clauses.append(f"(geo_locations ->> 'name') = {args['geo_scale']}")
        if platform == "facebook" and "language" in args:
            if args["language"] == "all":
                # no language targeted at all
                where_clauses.append("(all_fields -> 'languages' -> 'values') is null")
            else:
                # only a *single* language targeted
                where_clauses.append(
                    "jsonb_array_length(all_fields -> 'languages' -> 'values') = 1"
                )
                # specify that language
                lang = language_map[args["language"]]
                where_clauses.append(
                    f"(all_fields #>'{{languages, values}}') @> '[{lang}]'"
                )

        sql_query = sql_query + " AND\n ".join(where_clauses)

        # query database
        try:
            conn = psycopg2.connect(
                host='postgres',
                database="gbd2019",
                user=os.environ.get("POSTGRES_USER"),
                password=os.environ.get("POSTGRES_PASSWORD"),
            )
            df = pd.read_sql(sql_query, conn)
            conn.close()
        except:
            status = 500
            message = "Internal Server Error: Error returned from PostgreSQL server on SELECT."

        if status == 200:
            if df.shape[0] == 0:
                return {
                    "status": status,
                    "message": "OK: No data match this query.",
                    "timestamp": timestr(),
                    "data": None,
                }

            data = {
                "values": df.to_json(orient="values"),
                "columns": list(df.columns),
            }
            message = "OK: Data successfully selected from database."

    # return result
    time_end = datetime.datetime.now()
    delta = time_end - time_start
    return {
        "status": status,
        "message": message,
        "timestamp": timestr(),
        "data": data,
        "duration": delta.total_seconds(),
    }


