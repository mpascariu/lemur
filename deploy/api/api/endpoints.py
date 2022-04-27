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

        # create sql query
        # regions = str(args['region']).replace('[', '(').replace(']', ')')
        # sql_query = 'SELECT * FROM cod WHERE region IN {};'.format(regions)
        sql_query = "SELECT * FROM cod WHERE region = '{}';".format(args['region'])

        # query database
        try:
            conn = psycopg2.connect(
                host='postgres',
                database="gbd2019",
                user='lemur',
                password='tx*Oj3HjwAlNbNY0XrY3288E#',
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


def lt_fun(args):
    """Process requests to API endpoint '/life_table' by selecting queried data from a PostgreSQL table.
    Args:
        args (dict): Arguments of GET request passed from request.args
    Returns:
        dict: http response compatible with json format
    """
    time_start = datetime.datetime.now()

    status = 200
    message = 'OK'
    data = None

    time_end = datetime.datetime.now()
    delta = time_end - time_start

    return {
        "status": status,
        "message": message,
        "timestamp": timestr(),
        "data": data,
        "duration": delta.total_seconds(),
    }

def sdg_fun(args):
    """Process requests to API endpoint '/sdg' by selecting queried data from a PostgreSQL table.
    Args:
        args (dict): Arguments of GET request passed from request.args
    Returns:
        dict: http response compatible with json format
    """
    time_start = datetime.datetime.now()

    status = 200
    message = 'OK'
    data = None

    time_end = datetime.datetime.now()
    delta = time_end - time_start

    return {
        "status": status,
        "message": message,
        "timestamp": timestr(),
        "data": data,
        "duration": delta.total_seconds(),
    }
