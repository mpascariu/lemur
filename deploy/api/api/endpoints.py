#!/usr/bin/python3

import datetime
from api.utils import check_args, query


# query cause_of_death endpoint
def api_fun(args, table):
    """Process requests to API endpoint '/cause_of_death' by selecting queried data from a PostgreSQL table.
    Args:
        args (dict): Arguments of GET request passed from request.args
        table (str): Name of table to query
    Returns:
        dict: http response compatible with json format
    """
    time_start = datetime.datetime.now()

    # check arguments
    result = check_args(
        args,
        required=[],
        required_oneof=[],
        optional=['region', 'age', 'sex', 'year'],
    )
    args = result.get("args")
    status = result.get("status")

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

        # key to column name
        col = {'region': 'region',
               'age': 'x',
               'sex': 'sex',
               'year': 'period'}

        # create sql query
        sql_query = 'SELECT * FROM ' + table

        # where statements
        where_statements = []
        for key in list(args.keys()):
            if key == 'region':
                where_statements.append(col[key] + ' IN ' + str(args[key]).replace('[', '(').replace(']', ')'))
            else:
                where_statements.append(col[key] + ' = ' + str(args[key]))
        if len(where_statements) > 0:
            sql_query = sql_query + ' WHERE {}'.format(' AND '.join(where_statements))

        # finish query
        sql_query = sql_query + ';'
        print(sql_query)

        # query database
        result = query(sql_query)

    # time elapsed
    # result['duration'] = (datetime.datetime.now() - time_start).total_seconds()

    # return result
    return result


