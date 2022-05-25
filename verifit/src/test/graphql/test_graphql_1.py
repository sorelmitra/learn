from src.test.lib.runner import *


def test_graphql_1():
	expected, actual = runner.graphql()
	assert actual == expected
