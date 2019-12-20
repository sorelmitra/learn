from verifit import *

def test():
	name = "jsonplaceholder-post-1"
	input_filename = script_path(f"{name}.json")
	output_filename = script_path(f"{name}-answer.json")
	expected_output_filename = script_path(f"{name}-expected.json")

	command = [
		"curl", 
		"-X", "POST", 
		"https://jsonplaceholder.typicode.com/posts", 
		"--header", "Content-Type: application/json; charset=UTF-8", 
		"--data-binary", f"@{input_filename}", 
		"-o", output_filename
	]

	expected, got = run_test(command, input_filename, output_filename, expected_output_filename)
	assert expected == got
