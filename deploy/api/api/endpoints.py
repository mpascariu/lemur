#!/usr/bin/python3

import datetime
from io import StringIO

from api.utils import check_args, validate, query, timestr
from pandas import read_json

# Tables an endpoint may name. The table is chosen by the route (app.py), not
# by the caller, but it is interpolated into the SQL text rather than bound,
# so it is checked against this set before use.
ALLOWED_TABLES = frozenset({"cod", "sdg", "lt"})


# query cause_of_death endpoint
def api_fun(args, table, ip):
    """Process requests to API endpoint '/cause_of_death' by selecting queried data from a PostgreSQL table.
    Args:
        args (dict): Arguments of GET request passed from request.args
        table (str): Name of table to query
        ip (str): Requesting IP address
    Returns:
        dict: http response compatible with json format
    """

    # check arguments
    result = check_args(
        args,
        required=[],
        required_oneof=['region', 'age', 'sex', 'year'],
        optional=[],
    )
    args = result.get("args")
    status = result.get("status")

    if status == 200:

        # validate token
        result = validate(ip)
        status = result.get("status")

    if status == 200:

        # key to column name
        col = {'region': 'region',
               'age': 'x',
               'sex': 'sex',
               'year': 'period'}

        if table not in ALLOWED_TABLES:
            raise ValueError("unknown table: {!r}".format(table))

        # Build the statement from fixed text only: the table comes from the
        # allow-list above and the column names from `col`. Every value from
        # the request becomes a bind parameter, so nothing the caller sends
        # is ever parsed as SQL.
        sql_query = 'SELECT * FROM ' + table

        where_statements = []
        params = []
        for key in list(args.keys()):
            if key == 'region':
                # check_args turned this into a Python list; psycopg adapts
                # it to an array, so ANY(%s) replaces the old IN (...) text.
                where_statements.append(col[key] + ' = ANY(%s)')
                params.append(list(args[key]))
            else:
                where_statements.append(col[key] + ' = %s')
                params.append(args[key])

        if len(where_statements) > 0:
            sql_query = sql_query + ' WHERE {}'.format(' AND '.join(where_statements))

        sql_query = sql_query + ';'

        # query database
        result = query(sql_query, params)

    # return result
    return result


# query regions endpoint
def regions_fun(ip):
    """Process requests to API endpoint '/regions' by selecting queried data from a PostgreSQL table.
    Args:
        ip (str): Requesting IP address
    Returns:
        dict: http response compatible with json format
    """

    # validate token
    result = validate(ip)
    status = result.get("status")

    if status == 200:
        result = query('select distinct(region) from cod;')

    # return result
    return result


# query regions endpoint
def requests_fun(date=None):
    """Return counts of API requests.

    `date` reaches this function straight from the query string. It used to be
    interpolated into the SQL text, which made this endpoint an unauthenticated
    injection point; it is now validated as a calendar date and bound.

    The per-IP breakdown has also been dropped. api_requests stores the
    address of every caller, and this endpoint is public, so returning the
    `ip` column published visitors' IP addresses to anyone who asked.
    Args:
        date (str): optional day to report on, formatted YYYY-MM-DD
    Returns:
        dict: http response compatible with json format
    """

    if date is None:
        sql_query = (
            'select date, sum(requests) as requests '
            'from api_requests group by date order by date desc;'
        )
        params = None
    else:
        try:
            day = datetime.datetime.strptime(str(date), "%Y-%m-%d").date()
        except (TypeError, ValueError):
            return {
                "status": 400,
                "message": "Bad Request: 'date' must be formatted YYYY-MM-DD.",
                "timestamp": timestr(),
                "data": "{}",
                "html": "",
            }
        sql_query = (
            'select date, sum(requests) as requests '
            'from api_requests where date = %s group by date;'
        )
        params = [day]

    result = query(sql_query, params)
    # StringIO: passing a bare string to read_json is removed in pandas 3.
    result['html'] = read_json(StringIO(result.get('data'))).to_html()

    # return result
    return result
