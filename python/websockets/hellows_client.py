#!/usr/bin/env python

# WS client example

import os
import asyncio
import websockets

import constants

class Client:

	def __init__(self, *args, **kwargs):
		super().__init__(*args, **kwargs)
		self.__stop = False
		self.__websocket = None

	async def __do_action(self, name):
		if name.lower() == "stop":
			self.__stop = True
			return True

		if name.lower() == "restart":
			if self.__websocket != None:
				await self.__websocket.close()
			try:
				self.__websocket = await websockets.connect('ws://localhost:8765')
			except OSError as e:
				print(f"Could not connect: {e}")
			return True

		if name.lower() == "kill":
			await os._exit(0)
			return True
		
		return False

	async def hello(self):
		await self.__do_action("restart")
		while not self.__stop:
			try:
				name = input("What's your name? ")

				if await self.__do_action(name):
					continue

				await self.__websocket.send(name)
				print(f"> {name}")

				greeting = await self.__websocket.recv()
				print(f"< {greeting}")
			except websockets.exceptions.ConnectionClosed as e:
				if e.code == constants.WebSocketCloseCodes.CLOSE_NORMAL:
					print(f"< (server closed)")
					self.__stop = True
				else:
					print(f"< (server connection closed with code {e.code})")
	
		print(f"Graceful exit")
		await self.__websocket.close()

c = Client()
asyncio.get_event_loop().run_until_complete(c.hello())
