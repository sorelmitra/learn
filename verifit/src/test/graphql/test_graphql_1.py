from verifit import *


def test_graphql_1():
	command = [
		"vitgql", "send",
		"--input-file", get_input_filename(),
		"--output-file", get_output_filename(),
		"--server", "https://graphqlzero.almansi.me/api"
	]

	expected, actual = run_test(command, update_snapshot=True)
	assert_equals_ignore_whitespaces(expected, actual)
