from verifit import *
import re, string

apiKey = "18yKKTwlod4V7bSRAK2wK2Vn1GObHPV13nUMXx4w"
server = "39ijt5vs1a.execute-api.us-east-1.amazonaws.com/default"
path = "pizzemQueryPizza"

def test_query_pizza():
	name = "query_pizza"

	command = [
		"curl", 
		"-X", "GET",
		"-H", f"x-api-key: {apiKey}",
		"-H", "Content-Type: application/json",
		f"https://{server}/{path}",
		"-o", get_output_filename(name)
	]

	expected, actual = run_test(command, name)
	assert re.sub("\s+", "", expected) == re.sub("\s+", "", actual)