import inspect, os, subprocess, sys

def script_path(filename):
	#dir = sys.path[0] # uses Python Path entries, not reliable
	#dir = "test/jsonplaceholder" # Hardcoded
	# First index is stack function, second index is file name.
	# Partially reliable, works only if
	# - this function is called from within another function from this file,
	#   which in turn is called directly from the test file.
	# Best solution I have so far.
	dir = os.path.dirname(inspect.stack()[2][1])
	return os.path.join(dir, filename)

def load_file_as_string(filepath):
	with open(filepath) as f:
		content = f.read()
	return content

def run_command(command):
	subprocess.run(command)

def start_command(command):
	child = subprocess.Popen(command)
	return child

def get_other_filename(filename):
	return script_path(f"{filename}")

def get_input_filename(name):
	return script_path(f"{name}.json")

def get_output_filename(name):
	return script_path(f"{name}-answer.json")

def get_expected_output_filename(name):
	return script_path(f"{name}-expected.json")

def run_test(command, name):
	input_filename = script_path(f"{name}.json")
	output_filename = script_path(f"{name}-answer.json")
	expected_output_filename = script_path(f"{name}-expected.json")
	try:
		os.unlink(output_filename)
	except FileNotFoundError:
		pass
	run_command(command)
	got = load_file_as_string(output_filename)
	expected = load_file_as_string(expected_output_filename)
	return (expected, got)

def run_triggered_background_test(background_test_command, trigger_command, name):
	input_filename = script_path(f"{name}.json")
	output_filename = script_path(f"{name}-answer.json")
	expected_output_filename = script_path(f"{name}-expected.json")
	try:
		os.unlink(output_filename)
	except FileNotFoundError:
		pass
	background_test = start_command(background_test_command)
	run_command(trigger_command)
	background_test.wait()
	got = load_file_as_string(output_filename)
	expected = load_file_as_string(expected_output_filename)
	return (expected, got)

