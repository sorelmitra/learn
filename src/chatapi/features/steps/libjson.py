import json

def loadJsonFromDataFile(filepath):
	with open(f"features/data/{filepath}") as json_file:  
	    data = json.load(json_file)
	return data

def areJsonsEqual(a, b):
    a, b = json.dumps(a, sort_keys=True), json.dumps(b, sort_keys=True)
    return a == b
