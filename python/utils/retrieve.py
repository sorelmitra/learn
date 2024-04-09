import json
import requests
from python_graphql_client import GraphqlClient


#
# Sample code for fetching HTTP or GraphQL
#


def retrieve_graphql(url=None, query=None, variables=None, auth=None, log_prefix='Do a GraphQL query'):
    client = GraphqlClient(endpoint=url, auth=auth)
    prefix = f"{log_prefix} to {url}"
    print(prefix, 'variables', variables)
    response = client.execute(query, variables)
    print(prefix, 'response', response)
    return response


def retrieve_http(url=None, method=requests.get, log_prefix='Do a HTTP GET request', 
                  use_default_headers=True, auth=None, extra_headers={},
                  payload=None):
    prefix = f"{log_prefix} to {url}"

    def get_default_headers():
        headers = {}
        if use_default_headers:
            headers.update({
                'Content-Type': 'application/json'
            })
            if auth:
                headers['Authorization'] = auth
        headers.update(extra_headers)
        print(prefix, 'headers', headers)
        return headers

    print(prefix, 'payload', payload)
    response = method(
        url=url,
        data=None if payload is None else json.dumps(payload),
        headers=get_default_headers(),
    )
    print(prefix, 'response', response)
    data = response.json()
    print(prefix, 'JSON response', data)
    return data
