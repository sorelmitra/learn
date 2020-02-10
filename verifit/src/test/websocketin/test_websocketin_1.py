from verifit import *

def test_websocketin_1():
	name = "websocketin-1"

	with open(get_other_filename("websocketin-token.txt"), 'r') as f:
		token = f.read()

	trigger_command = [
		"vitwss", "send",
		"--input-file", get_input_filename(name), 
		f"wss://connect.websocket.in/v2/1998?token={token}"
	]

	background_test_command = [
		"vitwss", "receive",
		"--packets-to-receive", "1",
		"--wait-ms", "10000", 
		"--output-file", get_output_filename(name),
		f"wss://connect.websocket.in/v2/1998?token={token}"
		]
	
	expected, got = run_triggered_background_test(
		background_test_command, trigger_command, name)
	assert expected == got

