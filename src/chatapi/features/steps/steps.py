from librest import *
from libjson import *

@given(u'Chat API Host "{host}"')
def step_impl(context, host):
    global chatApiHost
    chatApiHost = host

@when(u'"{method}" "{jsonFile}" to "{path}"')
def step_impl(context, method, jsonFile, path):
    global receivedJson
    global status
    json = loadJsonFromDataFile(jsonFile)
    url = f"{chatApiHost}/{path}"
    (status, receivedJson) = launchRestMethodCall(method, url, json)

@then(u'Status is "{expectedStatus}" and response is "{jsonFile}"')
def step_impl(context, expectedStatus, jsonFile):
    global receivedJson
    global status
    expectedJson = loadJsonFromDataFile(jsonFile)
    assert(expectedStatus == status)
    assert(areJsonsEqual(expectedJson, receivedJson))
