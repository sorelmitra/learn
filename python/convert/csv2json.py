import sys, csv, json

csvFilePath = sys.argv[1]
jsonFilePath = sys.argv[2]

data = []
with open(csvFilePath) as csvFile:
    csvReader = csv.DictReader(csvFile)
    for row in csvReader:
        data.append(row)

with open(jsonFilePath, 'w') as jsonFile:
    jsonFile.write(json.dumps(data, indent=4))
