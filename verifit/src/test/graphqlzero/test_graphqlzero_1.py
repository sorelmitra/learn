from verifit import *


def test_graphqlzero_1():
	command = [
		"vitgql",
		get_input_filename(),
		"-o", get_output_filename()
	]

	expected, actual = run_test(command)
	assert expected == actual


