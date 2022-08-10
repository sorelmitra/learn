from runner import *


def test_websockets_1():
	expected, actual = runner.websocket(ignore_messages=['ping', 'Hello world!'])
	assert expected == actual

