from chatapi.utils.websocket import *

def websocketConnect(server):
	global client
	client = WebSocketClient(url=server)
	asyncio.get_event_loop().run_until_complete(client.connect())

def websocketSend(data):
	global client
	asyncio.get_event_loop().run_until_complete(client.send(data))

async def recvWithTimeout(timeout):
	try:
		return await asyncio.wait_for(client.recv(), timeout=3)
	except asyncio.TimeoutError:
		return None

def websocketRecv():
	global client
	return asyncio.get_event_loop().run_until_complete(recvWithTimeout(3))
