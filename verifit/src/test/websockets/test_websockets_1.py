from runner import *


def test_websockets_1():
	expected, actual = runner.websocket()
	assert expected == actual

