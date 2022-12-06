import json
import os
import sys
import boto3

from http.client import HTTPSConnection, HTTPConnection
from pprint import pprint
from urllib.parse import urlparse

from boto3 import Session
from botocore.auth import SigV4Auth
from botocore.awsrequest import AWSRequest
from botocore.credentials import Credentials


class AWSBase:
    def __init__(self, environ):
        self._session = boto3.Session(profile_name=f'{environ}')


class AWSElasticSearchHttpQuery(AWSBase):
    def __init__(self, environ, endpoint, index):
        super().__init__(environ)
        self.endpoint = endpoint
        self.index = index

    def find_users_exact(self, field_name, field_value):
        response = self._sigv4_request(
            f'/{self.index}/_search',
            method='GET',
            headers={
                'Content-Type': 'application/json',
            },
            body=json.dumps({
                "query": {
                    "bool": {
                        "must": [
                            {
                                "match_phrase": {
                                    f"{field_name}": f"{field_value}"
                                }
                            }
                        ]
                    }
                }
            }),
            region='us-west-2'
        )
        print(response.status)
        print(json.loads(response.read().decode()))

    def _sigv4_request(
            self,
            path,
            method='GET',
            body=None,
            params=None,
            headers=None,
            service='es',
            region='us-west-2',
            credentials=None
    ):
        """Sends an HTTP request signed with SigV4
        Args:
          path: The request URL (e.g. 'https://www.example.com').
          method: The request method (e.g. 'GET', 'POST', 'PUT', 'DELETE'). Defaults to 'GET'.
          body: The request body (e.g. json.dumps({ 'foo': 'bar' })). Defaults to None.
          params: The request query params (e.g. { 'foo': 'bar' }). Defaults to None.
          headers: The request headers (e.g. { 'content-type': 'application/json' }). Defaults to None.
          service: The AWS service name. Defaults to 'execute-api'.
          region: The AWS region id. Defaults to the env var 'AWS_REGION'.
          credentials: The AWS credentials. Defaults to the current boto3 session's credentials.
        Returns:
           The HTTP response
        """

        if credentials is None:
            credentials = self._session.get_credentials().get_frozen_credentials()

        # sign request
        req = AWSRequest(
            method=method,
            url='https://' + self.endpoint + path,
            data=body,
            params=params,
            headers=headers
        )
        print("Credentials", end=" ")
        pprint(credentials)
        SigV4Auth(credentials, service, region).add_auth(req)
        req = req.prepare()

        # parse URL
        u = urlparse(req.url)
        path_and_query = u.path if u.query is None else u.path + '?' + u.query

        # send request
        hostname = u.hostname or "missing-hostname"
        cnn = HTTPSConnection(hostname) if u.scheme == 'https' else HTTPConnection(hostname)
        cnn.request(
            req.method,
            path_and_query,
            headers=req.headers,
            body=req.body
        )
        return cnn.getresponse()


"""
Sends a HTTP request to AWS OpenSearch ElasticSearch instance.

Usage:
python <script>.py <AWS-environment> <ElasticSearch endpoint> <index> <operation> [<arguments>]

Where
  <script> is this script
  <AWS-environment>: the AWS environment to use, as it appears in your .aws/config file
  <ElasticSearch endpoint>: the Elastic Search AWS endpoint to use
  <index>: the Elastic Search index to use
  
  <operation> is one of
    find-exact: finds using 'match-phrase', has two <arguments>:
      first argument: field name to use
      second argument: field value to use 

Example: to find boats with the exact name "Spike" in the "boats" index from "my-elastic" endpoint
from the AWS "dev-profile", run:

python sigv4_using_http_client.py dev-profile my-elastic.us-west-2.es.amazonaws.com boats find-exact name Spike
"""
if __name__ == "__main__":
    data_es = AWSElasticSearchHttpQuery(sys.argv[1], sys.argv[2], sys.argv[3])
    if sys.argv[4] == "find-exact":
        data_es.find_users_exact(sys.argv[5], sys.argv[6])
    else:
        print("Invalid command")
