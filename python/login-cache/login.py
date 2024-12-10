import json
from datetime import datetime
from typing import Callable

from .cache import cache_set, cache_get
from .date_tools import date_subtract_in_minutes
from .json_web_token import decode_token, get_token_expiration_date


class LoginData:
    def __init__(self, *, access_token: str, expiry_date: datetime):
        self.access_token = access_token
        self.expiry_date = expiry_date

    def to_dict(self):
        return {
            "access_token": self.access_token,
            "expiry_date": self.expiry_date.isoformat() if self.expiry_date else None
        }

    @staticmethod
    def from_dict(data):
        return LoginData(
            access_token=data["access_token"],
            expiry_date=datetime.fromisoformat(data["expiry_date"]) if data["expiry_date"] else None
        )


def login(*, driver: Callable[[str, str], str], username, secret):
    access_token = driver(username, secret)
    decoded_token = decode_token(access_token)
    token_expiry_date = get_token_expiration_date(decoded_token)
    login_data = LoginData(access_token=access_token, expiry_date=token_expiry_date)
    cache_set(username)(login_data.to_dict())
    return login_data


def get_login_values_from_cache(username):
    login_data = LoginData.from_dict(cache_get(username))
    if login_data is None:
        print(f"Cache miss key <{username}>, no cached data")
        return None
    if login_data.expiry_date is None:
        print(f"Cache miss key <{username}>, no expiration info")
        return None

    minutes = date_subtract_in_minutes(
        from_date=login_data.expiry_date,
        date_to_subtract=datetime.now())
    if minutes < 5:
        print(f"Cache miss key <{username}>, almost expired")
        return None

    print(f"Cache hit key <{username}>")
    return login_data


def login_from_cache(*, driver: Callable[[str, str], str], username, secret):
    login_data = get_login_values_from_cache(username)
    if login_data is None:
        login_data = login(driver=driver, username=username, secret=secret)
    print(f"Caching login data <{login_data}>")
    return login_data

