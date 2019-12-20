import inspect, os, subprocess, sys

def script_path(filename):
	return os.path.join(sys.path[0], filename)

def load_file_as_string(filepath):
	with open(filepath) as f:
		content = f.read()
	return content

def run_command(command):
	subprocess.run(command)

def run_test(command, input_filename, output_filename, expected_output_filename):
	try:
		os.unlink(output_filename)
	except FileNotFoundError:
		pass
	run_command(command)
	got = load_file_as_string(output_filename)
	expected = load_file_as_string(expected_output_filename)
	return (expected, got)
