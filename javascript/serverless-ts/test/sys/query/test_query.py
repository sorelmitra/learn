from verifit import *
import re, string

apiKey = "18yKKTwlod4V7bSRAK2wK2Vn1GObHPV13nUMXx4w"
server = "https://5jej7wt976.execute-api.us-east-1.amazonaws.com/DEV"
common_command = [
	"curl", 
	"-X", "GET",
	"-H", f"x-api-key: {apiKey}",
	"-H", "Content-Type: application/json",
]

def test_query_pizza():
	expected, actual = run_test(common_command + [
		f"{server}/pizzas",
		"-o", get_output_filename(),
	])
	assert re.sub("\s+", "", expected) == re.sub("\s+", "", actual)

def test_query_pizza_by_type():
	expected, actual = run_test(common_command + [
		f"{server}/pizzas/:carbonara",
		"-o", get_output_filename(),
	])
	assert re.sub("\s+", "", expected) == re.sub("\s+", "", actual)
