from chatapi.utils.websocket import *

clients = {}

def websocketConnect(socketName, server):
	global clients
	clients[socketName] = WebSocketClient(url=server)
	asyncio.get_event_loop().run_until_complete(clients[socketName].connect())

def websocketSend(socketName, data):
	global clients
	asyncio.get_event_loop().run_until_complete(clients[socketName].send(data))

async def recvWithTimeout(socketName, timeout):
	try:
		return await asyncio.wait_for(clients[socketName].recv(), timeout=3)
	except asyncio.TimeoutError:
		return None

def websocketRecv(socketName):
	global clients
	return asyncio.get_event_loop().run_until_complete(recvWithTimeout(socketName, 3))
