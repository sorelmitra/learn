import os
from functools import cache
from multiprocessing import Process
import time
import queue
from flask import Flask, request, json, jsonify
from ariadne import load_schema_from_path, make_executable_schema, \
    graphql_sync, snake_case_fallback_resolvers, ObjectType, convert_kwargs_to_snake_case


#
# Sample HTTP server with a few endpoints and GraphQL
#


def get_script_dir():
    return os.path.dirname(__file__)


@cache
def api():
    return Flask('simple-http-server')


class HttpConfig:
    PORT = 5768
    POST_PATH = '/create'
    GET_PATH = '/obtain'
    PUT_PATH = '/update'
    PATCH_PATH = '/patch'
    DELETE_PATH = '/delete'
    GRAPHQL_PATH = '/graphql'

    @staticmethod
    def base_url():
        return f"http://localhost:{HttpConfig.PORT}"

    @staticmethod
    def post_url():
        return f"{HttpConfig.base_url()}{HttpConfig.POST_PATH}"

    @staticmethod
    def get_url():
        return f"{HttpConfig.base_url()}{HttpConfig.GET_PATH}"

    @staticmethod
    def put_url():
        return f"{HttpConfig.base_url()}{HttpConfig.PUT_PATH}"

    @staticmethod
    def patch_url():
        return f"{HttpConfig.base_url()}{HttpConfig.PATCH_PATH}"

    @staticmethod
    def delete_url():
        return f"{HttpConfig.base_url()}{HttpConfig.DELETE_PATH}"

    @staticmethod
    def graphql_url():
        return f"{HttpConfig.base_url()}{HttpConfig.GRAPHQL_PATH}"


KEY_QUEUE = 'queue'
KEY_ENDPOINT_INDEX = 'endpoint_index'
KEY_ENDPOINT_RESULTS = 'endpoint_results'
KEY_ENDPOINT_PREVIOUS_CALL_FAILED = 'endpoint_previous_call_failed'
KEY_ENDPOINT_CURRENT_PAYLOAD = 'endpoint_current_payload'
KEY_GRAPHQL_SCHEMA = 'graphql_schema'


@cache
def get_store():
    return {}


class EndpointResult:
    def __init__(self, *, status_code=200, response_data=None):
        self.status_code = status_code
        self.response_data = response_data

    def is_successful(self):
        return 200 <= self.status_code <= 299


def determine_endpoint_result() -> EndpointResult:
    store = get_store()
    endpoint_results: list[EndpointResult] = store.get(KEY_ENDPOINT_RESULTS)
    endpoint_index = store.get(KEY_ENDPOINT_INDEX)

    if endpoint_index < len(endpoint_results):
        endpoint_result = endpoint_results[endpoint_index]
    else:
        current_payload = store.get(KEY_ENDPOINT_CURRENT_PAYLOAD, None)
        endpoint_result = EndpointResult(
            status_code=200,
            response_data=current_payload if current_payload is not None else {"success": True}
        )

    if store.get(KEY_ENDPOINT_PREVIOUS_CALL_FAILED) and endpoint_result.is_successful():
        endpoint_result.status_code = 202

    if not endpoint_result.is_successful():
        store[KEY_ENDPOINT_PREVIOUS_CALL_FAILED] = True

    print(f"Endpoint {endpoint_index} Result: {endpoint_result.status_code} {endpoint_result.response_data}")

    endpoint_index = endpoint_index + 1
    store[KEY_ENDPOINT_INDEX] = endpoint_index
    return endpoint_result


@api().route(HttpConfig.POST_PATH, methods=['POST'])
def post_listener():
    store = get_store()
    q = store.get(KEY_QUEUE)
    post_payload = request.json
    print(f"Post Payload: {post_payload}")
    store[KEY_ENDPOINT_CURRENT_PAYLOAD] = post_payload
    endpoint_result = determine_endpoint_result()
    q.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


@api().route(HttpConfig.GET_PATH, methods=['GET'])
def get_listener():
    store = get_store()
    q = store.get(KEY_QUEUE)
    endpoint_result = determine_endpoint_result()
    q.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


@api().route(HttpConfig.PUT_PATH, methods=['PUT'])
def put_listener():
    store = get_store()
    q = store.get(KEY_QUEUE)
    put_payload = request.json
    print(f"Put Payload: {put_payload}")
    store[KEY_ENDPOINT_CURRENT_PAYLOAD] = put_payload
    endpoint_result = determine_endpoint_result()
    q.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


@api().route(HttpConfig.PATCH_PATH, methods=['PATCH'])
def patch_listener():
    store = get_store()
    q = store.get(KEY_QUEUE)
    patch_payload = request.json
    print(f"Patch Payload: {patch_payload}")
    store[KEY_ENDPOINT_CURRENT_PAYLOAD] = patch_payload
    endpoint_result = determine_endpoint_result()
    q.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


@api().route(HttpConfig.DELETE_PATH, methods=['DELETE'])
def delete_listener():
    store = get_store()
    q = store.get(KEY_QUEUE)
    endpoint_result = determine_endpoint_result()
    q.put(endpoint_result.response_data)
    return json.dumps(endpoint_result.response_data), endpoint_result.status_code


@api().route(HttpConfig.GRAPHQL_PATH, methods=["POST"])
def graphql_server():
    store = get_store()
    schema = store.get(KEY_GRAPHQL_SCHEMA)
    data = request.get_json()
    success, result = graphql_sync(
        schema,
        data,
        context_value=request,
        debug=api().debug
    )
    status_code = 200 if success else 400
    return jsonify(result), status_code


@convert_kwargs_to_snake_case
def get_notice_resolver(_obj, _info, id):
    endpoint_result = determine_endpoint_result()
    if endpoint_result.is_successful():
        payload = {
            "success": True,
            "notice": {
                "id": id,
                "title": 'foo notice'
            }
        }
    else:
        payload = {
            "success": False,
            "errors": [
                endpoint_result.response_data if endpoint_result.response_data is not None
                else f"Notice item matching {id} not found"]
        }
    print(f"GraphQL Get Notice Payload: {payload}")
    return payload


@convert_kwargs_to_snake_case
def create_notice_resolver(_obj, _info, title):
    endpoint_result = determine_endpoint_result()
    if endpoint_result.is_successful():
        payload = {
            "success": True,
            "notice": {
                "id": 'foo',
                "title": title
            }
        }
    else:
        payload = {
            "success": False,
            "errors": [f"Notice item not created"]
        }
    print(f"GraphQL Create Notice Payload: {payload}")
    return payload


def http_server_start_process(
        response_queue, endpoint_results: list[EndpointResult], graphql_schema):
    store = get_store()
    store[KEY_QUEUE] = response_queue
    store[KEY_ENDPOINT_INDEX] = 0
    store[KEY_ENDPOINT_RESULTS] = [] if endpoint_results is None else endpoint_results

    if graphql_schema is None:
        type_definitions = load_schema_from_path(os.path.join(get_script_dir(), "schema.graphql"))
        query = ObjectType("Query")
        query.set_field("getNotice", get_notice_resolver)
        mutation = ObjectType("Mutation")
        mutation.set_field("createNotice", create_notice_resolver)
        store[KEY_GRAPHQL_SCHEMA] = make_executable_schema(
            type_definitions, query, mutation, snake_case_fallback_resolvers
        )
    else:
        store[KEY_GRAPHQL_SCHEMA] = graphql_schema()

    api().run(port=HttpConfig.PORT)


def http_server_start(
        *, response_queue, endpoint_results: list[EndpointResult]=None, graphql_schema=None):
    server = Process(
        target=http_server_start_process,
        args=(response_queue, endpoint_results, graphql_schema))
    server.start()
    time.sleep(2)
    return server


def http_server_stop(server):
    server.terminate()
    server.join()
    server.close()


def queue_is_empty(q):
    try:
        q.get(timeout=1)
        return False
    except queue.Empty:
        return True
    except:
        assert False
