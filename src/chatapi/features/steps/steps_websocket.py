from testlibassert import *
from testlibjson import *

from testlibwebsocket import *

@when(u'WebSocket {socketName} Connect to Server "{server}"')
def step_impl(context, socketName, server):
    websocketConnect(socketName, server)

@when(u'WebSocket {socketName} Send "{jsonFilePath}"')
def step_impl(context, socketName, jsonFilePath):
    with open(f"features/data/{jsonFilePath}") as jsonFile:
        json = jsonFile.read()
    websocketSend(socketName, json)

@then(u'WebSocket {socketName} Get response like "{reStr}"')
def step_impl(context, socketName, reStr):
    data = websocketRecv(socketName)
    sortedData = sortedJson(json.loads(data))
    assertAsStrContains(sortedData, reStr, "content")
