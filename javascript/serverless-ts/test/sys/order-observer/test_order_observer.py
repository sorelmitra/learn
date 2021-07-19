from verifit import *
import re

def test_order_observer():
	expected, actual = run_test([
		"aws", "lambda", "invoke",
		"--cli-binary-format", "raw-in-base64-out",
		"--function-name", "pizzem-order-observer",
		"--payload", f"file://{get_input_filename()}",
		get_output_filename()])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

