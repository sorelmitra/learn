import functools
import json


@functools.cache
def cache_store():
	"""
    Creates or retrieves a memoized cache store.

    Returns:
        dict: A dictionary used as a cache store.
    """
	return {}


def set_cache_filename(filename):
	"""
    Sets the filename for the cache file.

    Parameters:
        filename (str): The filename to be used for storing cache data.
    """
	cache_store()['filename'] = filename


def get_cache_filename():
	"""
    Retrieves the filename for the cache file.

    Returns:
        str: The filename for cache storage. Defaults to 'cache.json' if not set.
    """
	return cache_store().get('filename', 'cache.json')


def cache_read():
	"""
    Reads and returns the contents of the cache file.

    Returns:
        dict: The contents of the cache file. Returns an empty dictionary if the file doesn't exist
              or if there's a JSON decoding error.
    """
	try:
		with open(get_cache_filename(), 'r') as f:
			return json.load(f)
	except FileNotFoundError:
		return {}
	except json.JSONDecodeError:
		return {}


def cache_write(cache):
	"""
    Writes the given cache data to the cache file.

    Parameters:
        cache (dict): The cache data to write.
    """
	with open(get_cache_filename(), 'w') as f:
		json.dump(cache, f, indent=2)


def cache_set(key):
	"""
    Creates a decorator that assigns a value to a given key in the cache and writes it to the cache file.

    Parameters:
        key (str): The key under which to store the value.

    Returns:
        Function: A decorator function that takes a value, stores it under the given key in the cache,
                  and then writes the cache to the cache file.
    """

	def with_value(value):
		cache = cache_read()
		cache[key] = value
		cache_write(cache)
		return value

	return with_value


def cache_get(key):
	"""
    Retrieves the value associated with the given key from the cache.

    Parameters:
        key (str): The key for the value to retrieve.

    Returns:
        The value associated with the key, or None if the key does not exist.
    """
	cache = cache_read()
	return cache.get(key)


def retrieve_and_cache(key=None, arg=None, describe_func=None):
	"""
    Decorator for caching the result of a function call. If the cache misses, it retrieves the result by calling
    the function, caches it, and returns the result. Prints cache hit or miss information.

    Parameters:
        key (str): The cache key to use for storing the result.
        arg (any): An argument to pass to the function if the cache is missed. If None, the function is called without arguments.
        describe_func (function): A function that takes the response and returns a string description.

    Returns:
        Function: A decorator that wraps a function, caches its result using the specified key,
                  and prints cache hit/miss information along with a description of the result.
    """

	def with_func(func):
		resp = cache_get(key)
		if resp is None:
			print(f"Cache miss key <{key}>")
			retrieve_result = func() if arg is None else func(arg)
			cache_set(key)(retrieve_result)
		else:
			print(f"Cache hit key <{key}>")
		resp = cache_get(key)
		print(f"{key}: {describe_func(resp)}")
		return resp

	return with_func
