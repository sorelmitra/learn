import os
from functools import cache
from dotenv import dotenv_values
import pytest

#
# Sample code on how to write tests with drivers
#

@cache
def env_values():
    environment_name = os.environ.get('ENV', 'dev')
    os.environ['ENV'] = environment_name
    values = {
        **dotenv_values(f".{environment_name}.env"),
        **os.environ
    }
    return values



get_env = env_values().get


def get_driver():
    return get_env('DRIVER')


def call_driver(drivers):
    def with_args(**kwargs):
        driver = get_driver()
        if driver in drivers.keys():
            return drivers.get(driver)(**kwargs)
    return with_args


def driver_is_one_of(drivers):
    return get_driver() in drivers


#
# sample test with drivers
#

DRIVER_FOO = 'foo'
DRIVER_BAR = 'bar'
DRIVER_BAZ = 'baz'

def post(**kwargs):
    return call_driver({
        DRIVER_FOO: foo_post,
        DRIVER_BAR: bar_post,
    })(**kwargs)


def check_post_response(**kwargs):
    return call_driver({
        DRIVER_FOO: foo_check_post_response,
        DRIVER_BAR: bar_check_post_response,
    })(**kwargs)

@pytest.mark.skipif(driver_is_one_of([DRIVER_BAZ]), reason='bar cannot post')
def test_post_with_drivers():
    response = post(userId="2", title="a title", body="a body")
    check_post_response(response=response, userId="2")


