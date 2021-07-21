from verifit import *
import re, time

apiKey = "18yKKTwlod4V7bSRAK2wK2Vn1GObHPV13nUMXx4w"
server = "https://5jej7wt976.execute-api.us-east-1.amazonaws.com/DEV"
common_command = [
	"curl",
	"-X", "POST",
	"-H", f"x-api-key: {apiKey}",
	"-H" "Content-Type: application/json",
	f"{server}/pizzas",
]

def test_graphql_query():
	expected, actual = run_test(common_command + [
		"--data", f"@{get_input_filename()}",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_graphql_get_all_pizzas():
	expected, actual = run_test(common_command + [
		"--data", f"@{get_input_filename()}",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)

def test_graphql_get_pizzas_by_type():
	expected, actual = run_test(common_command + [
		"--data", f"@{get_input_filename()}",
		"-o", get_output_filename(),
	])
	assert re.sub(r'\s+', "", expected) == re.sub(r'\s+', "", actual)


