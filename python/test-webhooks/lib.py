import os
from functools import cache
from multiprocessing import Process
import time
import queue

from dotenv import dotenv_values
from flask import Flask, request, json

from verifit import EndpointResult, EndpointResultConfig, EndpointResultComputer


def get_script_dir():
    return os.path.dirname(__file__)


@cache
def env_values():
    environment_name = os.environ.get('ENV', 'dev')
    os.environ['ENV'] = environment_name
    values = {
        **dotenv_values(f".{environment_name}.env"),
        **os.environ
    }
    return values


@cache
def api():
    return Flask('simple-http-server')


class HttpConfig:
    PORT = 5768
    POST_PATH = '/create'

    @staticmethod
    def base_url():
        return f"http://localhost:{HttpConfig.PORT}"

    @staticmethod
    def post_url():
        return f"{HttpConfig.base_url()}{HttpConfig.POST_PATH}"


@cache
def get_store():
    return {}


@api().route(HttpConfig.POST_PATH, methods=['POST'])
def post_listener():
    config: EndpointResultConfig = get_store().get(EndpointResultConfig)
    result_computer = EndpointResultComputer(get_store())
    post_payload = request.json
    print(f"Post Payload: {post_payload}")
    endpoint_result = result_computer.next(post_payload)
    config.response_queue.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


def http_server_start_process(
        response_queue: queue, available_results: list[EndpointResult]):
    config = EndpointResultConfig(
        response_queue=response_queue, available_results=available_results)
    store = get_store()
    store[EndpointResultConfig] = config
    api().run(port=HttpConfig.PORT)


def http_server_start(
        *, response_queue: queue, available_results: list[EndpointResult]=None):
    server = Process(
        target=http_server_start_process,
        args=(response_queue, available_results))
    server.start()
    time.sleep(2)
    return server


def http_server_stop(server):
    server.terminate()
    server.join()
    server.close()


def queue_is_empty(q: queue):
    try:
        q.get(timeout=1)
        return False
    except queue.Empty:
        return True
    except:
        assert False
