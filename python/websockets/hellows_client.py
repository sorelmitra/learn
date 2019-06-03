#!/usr/bin/env python

# WS client example

import os
import asyncio
import websockets


async def is_action(websocket, name):
	if name.lower() == "close":
		await websocket.close()
		return True

	if name.lower() == "kill":
		await os._exit(0)
		return True

async def hello():
	async with websockets.connect(
			'ws://localhost:8765') as websocket:
		name = input("What's your name? ")

		if await is_action(websocket, name):
			return

		await websocket.send(name)
		print(f"> {name}")

		greeting = await websocket.recv()
		print(f"< {greeting}")

asyncio.get_event_loop().run_until_complete(hello())
