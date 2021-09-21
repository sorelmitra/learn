from verifit import *


def test_websocketin_1():
	token = r'oCdCMcMPQpbvNjUIzqtvF1d2X2okWpDQj4AwARJuAgtjhzKxVEjQU6IdCjwm&notify_self'
	server = f"wss://demo.piesocket.com/v3/channel_1?api_key={token}"

	trigger_command = [
		"vitwss", "send",
		"--input-file", get_input_filename(),
		server
	]

	background_test_command = [
		"vitwss", "receive",
		"--packets-to-receive", "1",
		"--wait-ms", "10000", 
		"--output-file", get_output_filename(),
		server
		]
	
	expected, actual = run_triggered_background_test(
		background_test_command, trigger_command)
	assert expected == actual

