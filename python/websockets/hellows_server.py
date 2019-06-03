#!/usr/bin/env python

# WS server example

import asyncio
import websockets

class WebSocketCloseCodes:
	CLOSE_NORMAL = 1000

async def get_action(name):
	global loop
	action = 'respond'
	if name.lower() == 'bye':
		server.close()
		action = 'exit'
	return action

async def hello(websocket, path):
	try:
		name = await websocket.recv()
		print(f"< {name}")

		action = await get_action(name)
		if action == 'exit':
			return

		greeting = f"Hello {name}!"

		await websocket.send(greeting)
		print(f"> {greeting}")
	except websockets.exceptions.ConnectionClosed as e:
		if e.code == WebSocketCloseCodes.CLOSE_NORMAL:
			print(f"< (client closed connection)")
		else:
			print(f"< (client connection died with code {e.code}")

async def run():
	global server
	server = await websockets.serve(hello, 'localhost', 8765)
	await server.wait_closed()

loop = asyncio.new_event_loop()
loop.run_until_complete(run())
