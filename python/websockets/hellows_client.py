#!/usr/bin/env python

# WS client example

import os
import asyncio
import websockets


async def do_action(websocket, name):
	action = name.lower()
	if name.lower() == "stop":
		return action

	if name.lower() == "close":
		await websocket.close()
		return action

	if name.lower() == "kill":
		await os._exit(0)
		return action
	
	return ''

async def hello():
	stop = False
	while not stop:
		async with websockets.connect(
				'ws://localhost:8765') as websocket:
			name = input("What's your name? ")

			action = await do_action(websocket, name)
			if action == 'stop':
				stop = True
				continue
			elif len(action) > 0:
				continue

			await websocket.send(name)
			print(f"> {name}")

			greeting = await websocket.recv()
			print(f"< {greeting}")

asyncio.get_event_loop().run_until_complete(hello())
