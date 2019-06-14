from testlibassert import *
from testlibjson import *

from testlibwebsocket import *

@when(u'WebSocket Connect to Server "{server}"')
def step_impl(context, server):
    websocketConnect(server)

@when(u'WebSocket Send "{jsonFilePath}"')
def step_impl(context, jsonFilePath):
    with open(f"features/data/{jsonFilePath}") as jsonFile:
        json = jsonFile.read()
    websocketSend(json)

@then(u'WebSocket Get response like "{reStr}"')
def step_impl(context, reStr):
    data = websocketRecv()
    sortedData = sortedJson(json.loads(data))
    assertAsStrContains(sortedData, reStr, "content")
