import os
from functools import cache

from dotenv import dotenv_values


@cache
def config():
    environment_name = os.environ.get('ENV', 'dev')
    os.environ['ENV'] = environment_name
    values = {
        **dotenv_values(f".{environment_name}.env"),
        **os.environ
    }
    return values


def get_env_reader():
    store = config()

    def get_env(key):
        return store.get(key)

    return get_env
