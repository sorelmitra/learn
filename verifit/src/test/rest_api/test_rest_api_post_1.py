from verifit import *


def test_rest_api_post_1():
	command = [
		"curl", 
		"-X", "POST", 
		"https://jsonplaceholder.typicode.com/posts", 
		"--header", "Content-Type: application/json; charset=UTF-8", 
		"--data-binary", f"@{get_input_filename()}",
		"-o", get_output_filename()
	]

	expected, actual = run_test(command)
	assert expected == actual
