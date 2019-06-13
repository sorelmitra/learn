from testlibassert import *
from testlibjson import *

from testlibrest import *

@given(u'Chat API Host "{host}"')
def step_impl(context, host):
    global chatApiHost
    chatApiHost = host

@when(u'"{method}" "{jsonFilePath}" to "{path}"')
def step_impl(context, method, jsonFilePath, path):
    global receivedJson
    global status
    with open(f"features/data/{jsonFilePath}") as jsonFile:
        json = jsonFile.read()
    path = expandGroups(path)
    url = f"{chatApiHost}{path}/"
    (status, receivedJson) = launchRestMethodCall(method, url, json)

@then(u'Status is "{expectedStatus}" and response is "{jsonFilePath}"')
def step_impl(context, expectedStatus, jsonFilePath):
    global receivedJson
    global status
    expectedJson = loadJsonFromDataFile(jsonFilePath)
    assertAsStrEqual(expectedStatus, status, "status")
    assertAsStrEqual(sortedJson(expectedJson), sortedJson(receivedJson), "content")

@then(u'Status is "{expectedStatus}" and response contains "{reStr}"')
def step_impl(context, expectedStatus, reStr):
    global receivedJson
    global status
    reStr = expandGroups(reStr)
    assertAsStrEqual(expectedStatus, status, "status")
    assertAsStrContains(sortedJson(receivedJson), reStr, "content")
