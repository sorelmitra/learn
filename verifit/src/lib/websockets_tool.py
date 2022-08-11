import asyncio

import websockets
from dotenv import dotenv_values

from verifit import *


class WebsocketsTool:
    def __init__(self,
                 server=None,
                 input_filename=None,
                 output_filename=None,
                 wait_ms=None,
                 ignore_list=None):
        self.LOG = print
        self._ENV = os.environ['ENV']
        self._config = {
            **dotenv_values(f".{self._ENV}.env")
        }

        self._server = server
        if server is None:
            self._server = self._config['WEBSOCKETS_SERVER_URL']

        self._input_filename = input_filename
        self._output_filename = output_filename

        self._wait_ms = wait_ms
        if wait_ms is None:
            self._wait_ms = int(self._config['WEBSOCKETS_WAIT_MS'])

        self._ignore_list = ignore_list

        self._packets_count = 1
        self._receive_handle = None

    def run(self):
        self._start_receiving()
        self._send()
        self._end_receiving()

    @staticmethod
    def _reduce_url_for_log(url):
        m = re.match(r'(.*api_key=).*', url)
        if m:
            return f"{m.group(1)}[...]"
        return url

    async def _send_async(self, data):
        async with websockets.connect(self._server) as websocket:
            await websocket.send(data)
            self.LOG(f"Sent data <{data}>")

    def _send_sync(self, data):
        asyncio.get_event_loop().run_until_complete(self._send_async(data))

    async def _receive_async(self):
        async with websockets.connect(self._server) as websocket:
            received = False
            while not received:
                data = await websocket.recv()
                self.LOG(f"Received data <{data}>")
                if data not in self._ignore_list:
                    received = True
        return data

    async def _receive_async_with_timeout(self):
        seconds = self._wait_ms / 1000
        try:
            result = await asyncio.wait_for(
                self._receive_async(),
                seconds
            )
            self.LOG(f"Receive async result <{result}>")
        except TimeoutError:
            result = f"Timed out waiting to receive data from {self._reduce_url_for_log(self._server)}"
        return result

    def _send(self):
        self.LOG(f"Sending contents of {self._input_filename} to {self._reduce_url_for_log(self._server)}")
        with open(self._input_filename, 'r') as my_file:
            data = my_file.read()
        self._send_sync(data)

    def _start_receiving(self):
        self.LOG(
            f"Start receiving {self._packets_count} packet(s) in {self._wait_ms} milliseconds from "
            f"{self._reduce_url_for_log(self._server)} to {self._output_filename}; ignoring {self._ignore_list}")
        self._receive_handle = asyncio.ensure_future(self._receive_async_with_timeout())

    def _end_receiving(self):
        data = asyncio.get_event_loop().run_until_complete(self._receive_handle)
        with open(self._output_filename, 'w') as my_file:
            my_file.write(data)
        self.LOG(f"Received {self._packets_count} packet(s) to {self._output_filename}")


