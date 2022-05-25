from verifit import *


def test_websockets_1():
	token = r'VCXCEuvhGcBDP7XhiJJUDvR1e1D3eiVjgZ9VRiaV'
	server = f"wss://demo.piesocket.com/v3/channel_1?api_key={token}&notify_self"

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

