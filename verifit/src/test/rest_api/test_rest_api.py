from runner import *


def test_rest_api_post_1():
    expected, actual = runner.rest(path='/posts', method='POST')
    assert actual == expected


def test_rest_api_get_sorted():
    expected, actual = runner.rest(path='/todos', method='GET', use_input_file=False,
                                   sort=[{'list': '', 'field': 'title'}]) # sort top list by title
    assert actual == expected


def test_rest_api_get_strip_regex():
    expected, actual = runner.rest(path='/todos/1', method='GET', use_input_file=False,
                                   strip_regex=[r'"id": (\d+),.*\n']) # strip id from response with regex
    assert actual == expected


def test_rest_api_get_strip_keys():
    expected, actual = runner.rest(path='/todos/1', method='GET', use_input_file=False,
                                   strip_keys=['id']) # strip id from response with keys
    assert actual == expected
