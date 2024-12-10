import json
import multiprocessing
import requests

from endpoint_tools import EndpointResult
from lib import http_server_start, http_server_stop, HttpConfig, queue_is_empty, env_values

get_env = env_values().get


def webhook_url():
    return f"http://host.docker.internal:{HttpConfig.PORT}{HttpConfig.POST_PATH}"


def trigger_webhook(payload):
    url = get_env('WEBHOOK_TRIGGER_URL')
    access_token = get_env('ACCESS_TOKEN')
    print(f"Triggering webhook to {url} with payload {payload}")
    resp = requests.post(url=url, data=json.dumps(payload), headers={
        'Content-Type': 'application/json',
        'Authorization': None if access_token is None else access_token
    }, timeout=10)
    return resp.status_code


def test_send_foo_succeeds():
    q = multiprocessing.Queue()
    server = http_server_start(response_queue=q)

    try:
        status = trigger_webhook(payload={
            'url': webhook_url(),
            'authHeaderKey': "Authorization",
            'authToken': "foo-token",
            'hookData': json.dumps({
                'foo': "bar"
            }),
            'threadType': "foo-type",
            'threadValue': "foo-id",
            'externalMessageId': "foo-ext-id",
            'request': 'abc'
        })

        assert status == 200
        webhook_response = q.get(timeout=1)
        assert webhook_response == {
            'foo': "bar"
        }
    finally:
        http_server_stop(server)


def test_send_foo_fails():
    q = multiprocessing.Queue()
    server = http_server_start(
        response_queue=q, available_results=[
            EndpointResult(status_code=406),
            EndpointResult(status_code=406),
            EndpointResult(status_code=406)
        ])

    try:
        status = trigger_webhook(payload={
            'url': webhook_url(),
            'authHeaderKey': "Authorization",
            'authToken': "foo-token",
            'hookData': json.dumps({
                'foo': "bar"
            }),
            'threadType': "foo-type",
            'threadValue': "foo-id",
            'externalMessageId': "foo-ext-id",
            'request': 'abc'
        })

        assert status == 200
        for _ in range(0, 3):
            webhook_response = q.get(timeout=1)
            assert webhook_response == { "success": False }
        assert queue_is_empty(q)
    finally:
        http_server_stop(server)


def test_send_foo_second_attempt_succeeds():
    q = multiprocessing.Queue()
    server = http_server_start(
        response_queue=q, available_results=[EndpointResult(status_code=406)])

    try:
        status = trigger_webhook(payload={
            'url': webhook_url(),
            'authHeaderKey': "Authorization",
            'authToken': "foo-token",
            'hookData': json.dumps({
                'foo': "bar"
            }),
            'threadType': "foo-type",
            'threadValue': "foo-id",
            'externalMessageId': "foo-ext-id",
            'request': 'abc'
        })

        assert status == 200
        # First attempt fails
        webhook_response = q.get(timeout=1)
        assert webhook_response == { "success": False }
        # Second attempt succeeds
        webhook_response = q.get(timeout=1)
        assert webhook_response == {
            'foo': "bar"
        }
        # No further calls to our webhook server
        assert queue_is_empty(q)
    finally:
        http_server_stop(server)

