from verifit import *

def test():
	name = "jsonplaceholder-post-1"

	command = [
		"curl", 
		"-X", "POST", 
		"https://jsonplaceholder.typicode.com/posts", 
		"--header", "Content-Type: application/json; charset=UTF-8", 
		"--data-binary", f"@{get_input_filename(name)}", 
		"-o", get_output_filename(name)
	]

	expected, got = run_test(command, name)
	assert expected == got
