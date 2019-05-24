import json

def loadJsonFromDataFile(filepath):
	with open(f"features/data/{filepath}") as jsonFile:
	    data = json.load(jsonFile)
	return data

def sortedJson(a):
    return json.dumps(a, sort_keys=True)
