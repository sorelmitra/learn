from runner import *


def test_graphql_1():
	expected, actual = runner.graphql()
	assert actual == expected


def test_graphql_placeholders():
	expected, actual = runner.graphql(variables={'ID': '7'})
	assert actual == expected
