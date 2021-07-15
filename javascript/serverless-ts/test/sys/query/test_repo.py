from verifit import *
import re, string

apiKey = "18yKKTwlod4V7bSRAK2wK2Vn1GObHPV13nUMXx4w"
server = "https://5jej7wt976.execute-api.us-east-1.amazonaws.com/DEV"
common_command = [
	"curl", 
	"-H", f"x-api-key: {apiKey}",
	"-H", "Content-Type: application/json",
]

def test_query_pizza():
	expected, actual = run_test(common_command + [
		"-X", "GET",
		f"{server}/pizzas",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_query_pizza_by_type():
	expected, actual = run_test(common_command + [
		"-X", "GET",
		f"{server}/pizzas/:carbonara",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_query_pizza_inexistent():
	expected, actual = run_test(common_command + [
		"-X", "GET",
		f"{server}/pizzas/:strawberries",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_create_pizza():
	expected, actual = run_test(common_command + [
		"-X", "POST",
		f"{server}/pizzas",
		"--data", f"@{get_input_filename()}",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_create_order():
	expected, actual = run_test(common_command + [
		"-X", "POST",
		f"{server}/orders",
		"--data", f"@{get_input_filename()}",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_delete_order():
	expected, actual = run_test(common_command + [
		"-X", "DELETE",
		f"{server}/orders/:1",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

