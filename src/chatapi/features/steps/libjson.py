import json

def loadJsonFromDataFile(filepath):
	with open(f"features/data/{filepath}") as json_file:  
	    data = json.load(json_file)
	return data

def sortedJson(a):
    return json.dumps(a, sort_keys=True)
