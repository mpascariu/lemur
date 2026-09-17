"""Tests for check_args() argument coercion.

check_args validates and coerces every query-string argument before it is
passed to the database as a bind parameter. The coercion is load-bearing: a
value that escapes it reaches psycopg in the wrong type and the query
silently matches nothing rather than failing.

`region` is the case that matters. It has to become a real list before
`ANY(%s)` is built, and a plain string is still iterable -- binding
"['Angola']" produces the list of characters ['[', "'", 'A', ...] and the
query returns zero rows. The API reports that as a successful empty result,
so nothing surfaces the mistake.
"""
import os

# utils.py refuses to import without a database password. Nothing here opens
# a connection; the value only has to exist.
os.environ.setdefault("LEMUR_DB_PASSWORD", "test")

import pytest

from api.utils import check_args

REQUIRED_ONEOF = ["region", "age", "sex", "year"]


def run(args):
    """Call check_args the way the endpoints do."""
    return check_args(
        dict(args), required=[], required_oneof=REQUIRED_ONEOF, optional=[]
    )


@pytest.mark.parametrize(
    "args",
    [
        {"region": "['Angola']"},
        {"region": "['Angola']", "year": "2023"},
        {"region": "['Angola']", "age": "0"},
        {"region": "['Angola']", "sex": "male"},
        {"region": "['Angola']", "year": "2023", "sex": "male", "age": "0"},
        {"region": "['Angola', 'Albania']", "year": "2023"},
    ],
)
def test_region_becomes_a_list_of_names(args):
    """region must be a list of region names, whatever else was asked for.

    Each of these returned a list of single characters before the fix: the
    type checks were an elif chain, so any request carrying year or age took
    the integer branch and never reached the list branch.
    """
    result = run(args)

    assert result["status"] == 200
    assert isinstance(result["args"]["region"], list)
    assert result["args"]["region"] == [
        p.strip().strip("'") for p in args["region"].strip("[]").split(",")
    ]


def test_bare_region_string_is_wrapped():
    """A scalar region is accepted and wrapped, as the docs describe."""
    result = run({"region": "'Angola'", "year": "2023"})

    assert result["status"] == 200
    assert result["args"]["region"] == ["Angola"]


def test_integer_arguments_are_stored_as_ints():
    """Values bound to the database must be ints, not the raw strings."""
    result = run({"region": "['Angola']", "year": "2023", "age": "10"})

    assert result["status"] == 200
    assert result["args"]["year"] == 2023
    assert result["args"]["age"] == 10
    assert isinstance(result["args"]["year"], int)


def test_bad_values_are_rejected_with_400():
    """A rejection in one check must survive the later checks running."""
    for args in (
        {"region": "['Angola']", "year": "abc"},
        {"region": "oops"},
        {"region": "['" + "A" * 600 + "']"},
        {},
    ):
        assert run(args)["status"] == 400, args


def test_string_values_are_not_quoted():
    """Values are bind parameters, so quotes would become part of the value."""
    result = run({"region": "['Angola']", "sex": "male"})

    assert result["status"] == 200
    assert result["args"]["sex"] == "male"
