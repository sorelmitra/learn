from functools import cache


class LogLevel:
	ERROR = 1
	WARN = 2
	INFO = 3
	DEBUG = 4
	TRACE = 5


@cache
def store():
	return {}


def set_log_level(level: LogLevel):
	store()[LogLevel] = level


def get_log_level():
	return store().get(LogLevel, LogLevel.WARN)


def debug(*args):
	if get_log_level() < LogLevel.DEBUG:
		return

	print('[DEBUG]', *args)


def debug_func(func, *args):
	if get_log_level() < LogLevel.DEBUG:
		return

	func(*args)
