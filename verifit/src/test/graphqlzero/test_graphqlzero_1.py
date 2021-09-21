from verifit import *

def test_websocketin_1():
	command = [
		"vitgql",
		get_input_filename(),
		"-o", get_output_filename()
	]

	expected, actual = run_test(command)
	assert expected == actual


