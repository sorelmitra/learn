import asyncio
import json

import websockets

from .config import get_env_reader

get_env = get_env_reader()

def ws_send_and_receive(server=None, ignore_list=None, data=None):

    wait_ms = int(get_env('WEBSOCKETS_WAIT_MS'))

    async def receive_async():
        async with websockets.connect(server) as websocket:
            print(f"Connected to server {server}")
            await websocket.send(json.dumps(data))
            print(f"Sent <{data}>")
            received = False
            while not received:
                received_data = await websocket.recv()
                if received_data in ignore_list:
                    print(f"Ignoring received message <{received_data}>")
                else:
                    received = True
        return received_data

    async def receive_async_with_timeout():
        seconds = wait_ms / 1000
        print(f"Sending and receiving for {seconds} sec from {server}")
        received_data = await asyncio.wait_for(
            receive_async(),
            seconds
        )
        print(f"Received <{received_data}>")
        return received_data

    def start_receiving():
        return asyncio.get_event_loop().run_until_complete(receive_async_with_timeout())


    return json.loads(start_receiving())
