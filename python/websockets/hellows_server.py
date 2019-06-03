#!/usr/bin/env python

# WS server example

import asyncio
import websockets

import constants

class HelloServer:

	def __init__(self, *args, **kwargs):
		super().__init__(*args, **kwargs)
		self.__server = None
		self.__stop = False

	def __do_action(self, name):
		if name.lower() == 'stop server':
			self.__server.close()
			self.__stop = True
			return True
		return False

	async def hello(self, websocket, path):
		print(f"< (client connected)")
		try:
			while not self.__stop:
				name = await websocket.recv()
				print(f"< {name}")

				if self.__do_action(name):
					continue

				greeting = f"Hello {name}!"

				await websocket.send(greeting)
				print(f"> {greeting}")
			if self.__stop:
				print(f"Told to exit")
				return
			print(f"< (client closed connection)")
		except websockets.exceptions.ConnectionClosed as e:
			if e.code == constants.WebSocketCloseCodes.CLOSE_NORMAL:
				print(f"< (client disconnected)")
			else:
				print(f"< (client connection closed with code {e.code})")

	async def run(self):
		self.__server = await websockets.serve(self.hello, 'localhost', 8765)
		await self.__server.wait_closed()

s = HelloServer()
loop = asyncio.new_event_loop()
loop.run_until_complete(s.run())
