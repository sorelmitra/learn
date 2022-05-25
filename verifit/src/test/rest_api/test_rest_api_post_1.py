from runner import *


def test_rest_api_post_1():
	expected, actual = runner.rest(path='/posts', method='POST')
	assert actual == expected
