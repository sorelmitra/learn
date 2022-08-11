import os

from dotenv import dotenv_values


class Config:
    _ENV = os.environ['ENV']
    value = {
        **dotenv_values(f".{_ENV}.env")
    }
