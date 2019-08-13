#!/usr/bin/env python

# WS client example

import os
import asyncio
import websockets

from .constants import *

class WebSocketClient:

	def __init__(self, *args, **kwargs):
		self.__websocket = None
		assert ParamNames.URL in kwargs
		self.__url = kwargs[ParamNames.URL]

	async def disconnect(self):
		if self.__websocket == None:
			return
		await self.__websocket.close()
		self.__websocket = None
	
	async def connect(self):
		try:
			self.__websocket = await websockets.connect(self.__url)
			print(f'Connected to {self.__url}')
		except OSError as e:
			print(f"Could not connect: {e}")

	async def send(self, data):
		try:
			await self.__websocket.send(data)
			print(f"> {data}")
		except websockets.exceptions.ConnectionClosed as e:
			self.__print_exception(e)

	async def recv(self):
		try:
			data = await self.__websocket.recv()
			print(f"< {data}")
			return data
		except websockets.exceptions.ConnectionClosed as e:
			self.__print_exception(e)

	def __print_exception(self, e):
		if e.code == WebSocketCloseCodes.CLOSE_NORMAL:
			print(f"< (server closed)")
			self.__stop = True
		else:
			print(f"< (server connection closed with code {e.code})")

