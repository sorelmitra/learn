import copy
import datetime
import inspect
import json
import os
import re
import subprocess
from shutil import copyfile

import pytest
from appium import webdriver
from selenium.common.exceptions import InvalidSessionIdException


###########################################################
#
# Logging Tools
#
###########################################################

class LogLevel:
    ERROR = 1
    WARNING = 2
    INFO = 3
    DEBUG = 4
    TRACE = 5


class Logger:
    LEVEL = LogLevel.INFO
    __level_strings__ = {
        LogLevel.ERROR: 'ERROR',
        LogLevel.WARNING: 'WARNING',
        LogLevel.INFO: 'INFO',
        LogLevel.DEBUG: 'DEBUG',
        LogLevel.TRACE: 'TRACE'
    }

    def __init__(self):
        pass

    def error(self, message):
        self.__log__(LogLevel.ERROR, message)

    def warning(self, message):
        self.__log__(LogLevel.WARNING, message)

    def info(self, message):
        self.__log__(LogLevel.INFO, message)

    def debug(self, message):
        self.__log__(LogLevel.DEBUG, message)

    def trace(self, message):
        self.__log__(LogLevel.TRACE, message)

    def __log__(self, level, message):
        if level > self.LEVEL:
            return
        print(f"[{self.__level_strings__[level]}] {message}")


LOG = Logger()


###########################################################
#
# Common Tools
#
###########################################################

stack_number = 1
stack_file_index = 1
stack_function_index = 3

data_file_type = 'json'

def get_stack_number():
    global stack_number
    return stack_number

def set_stack_number(number):
    global stack_number
    stack_number = number

def get_data_file_type():
    global data_file_type
    return data_file_type

def set_data_file_type(new_data_file_type):
    global data_file_type
    data_file_type = new_data_file_type

def script_path(filename):
    global stack_number
    global stack_file_index
    # Have tried several ways to get this path, including hardcoded and:
    # dir = sys.path[0] # uses Python Path entries, not reliable
    # This solution is based on reflection.
    # First index is stack function, second index is file name.
    # Partially reliable, works only if
    # - this function is called from within another function from this file,
    #   which in turn is called directly from the test file.
    # Best solution I have so far.
    current_dir = os.path.dirname(inspect.stack()[stack_number + 1][stack_file_index])
    return os.path.join(current_dir, filename)


class VerifitException(Exception):
    pass


def sort_dict_lists(filepath, dict_content, lists_to_sort):
    sorted_dict = copy.deepcopy(dict_content)
    for list_dict in lists_to_sort:
        # print("XXXXXXXX", 1, list_dict)
        list_name = list_dict['list']
        field = list_dict['field']
        keys = list_name.split('.')
        # print("XXXXXXXX", 2, "field", field, "keys", keys)
        inner_dict = sorted_dict
        key = None
        i = None
        for i in range(len(keys) - 1):
            # print("XXXXXXXX", '2b', i)
            key = keys[i]
            try:
                inner_dict = inner_dict.get(key)
                # print("XXXXXXXX", 3, "key", key, "inner_dict", inner_dict)
            except KeyError:
                # print("XXXXXXXX", 4, key)
                raise VerifitException(f"Could not find list {list_name} in {filepath}: missing key <{key}>")
        if i is not None:
            key = keys[i + 1]
        try:
            # print("XXXXXXXX", 5, "key", key, "inner_dict", inner_dict)
            if key is None:
                inner_dict = sorted(inner_dict, key=lambda x: x[field])
                sorted_dict = inner_dict
            else:
                inner_dict[key] = sorted(inner_dict[key], key=lambda x: x[field])
            # print("XXXXXXXX", 6, "inner_dict", inner_dict)
        except KeyError as e:
            raise VerifitException(f"Could not sort list {list_name} in {filepath}: missing field <{field}> in list")
    # print("XXXXXXXX", 8, sorted_dict)
    return sorted_dict


def do_strip_regex(string, regexes):
    for regex in regexes:
        string = re.sub(regex, '', string)
    return string


stripped_values = []


def do_strip_keys(dict_content, strip_keys):
    global stripped_values
    stripped_values = []
    # print("XXXXXXXX", 1, strip_keys)
    for compound_key in strip_keys:
        keys = compound_key.split('.')
        # print("XXXXXXXX", 2, keys)
        inner_dict = dict_content
        i = None
        for i in range(len(keys) - 1):
            key = keys[i]
            inner_dict = inner_dict.get(key)
        if i is None:
            last_key = keys[0]
        else:
            last_key = keys[i + 1]
        # print("XXXXXXXX", 3, last_key)
        stripped_values.append(inner_dict[last_key])
        del inner_dict[last_key]
        # print("XXXXXXXX", 4, stripped_values)
    return dict_content


def get_stripped_values():
    global stripped_values
    return stripped_values


def load_file_as_string(filepath, format=False, strip_regex=None, strip_keys=None, sort=None):
    with open(filepath) as f:
        content = f.read()
    if strip_regex is not None:
        content = do_strip_regex(content, strip_regex)
    if format and len(content) > 1:
        try:
            dict_content = json.loads(content)
            if sort is not None:
                dict_content = sort_dict_lists(filepath, dict_content, sort)
            if strip_keys is not None:
                dict_content = do_strip_keys(dict_content, strip_keys)
            formatted_content = json.dumps(dict_content, indent=2)
            content = formatted_content
        except VerifitException as e:
            raise e
        except json.decoder.JSONDecodeError as e:
            pass
    return content



###########################################################
#
# Tools for Tests Based on Commands and Output
#
###########################################################

def run_command(command):
    subprocess.run(command)


def start_command(command):
    child = subprocess.Popen(command)
    return child


def get_other_filename(filename):
    return script_path(f"{filename}")


def get_input_filename():
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    return script_path(f"{name}.{data_file_type}")


def get_output_filename():
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    return script_path(f"{name}-answer.{data_file_type}")


def get_expected_output_filename():
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    return script_path(f"{name}-expected.{data_file_type}")


def get_test_results(expected_output_filename, output_filename, update_snapshot, strip_regex, strip_keys, sort, use_expected_output):
    actual = load_file_as_string(output_filename, format=True, strip_regex=strip_regex, strip_keys=strip_keys, sort=sort)
    update_file_content(actual, output_filename)
    maybe_update_snapshot(output_filename, expected_output_filename, update_snapshot)
    expected = None
    if use_expected_output:
        expected = load_file_as_string(expected_output_filename)
    return expected, actual


def run_test(command=None, func=None, update_snapshot=False, strip_regex=None, strip_keys=None, sort=None, use_expected_output=True):
    global stack_number
    global stack_function_index
    if command is None and func is None:
        raise VerifitException("Either command or func must be specified")
    elif command is not None and func is not None:
        raise VerifitException("Only one of command or func must be specified")
    name = inspect.stack()[stack_number][stack_function_index]
    output_filename = script_path(f"{name}-answer.{data_file_type}")
    expected_output_filename = script_path(f"{name}-expected.{data_file_type}")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    if command is None:
        func()
    else:
        run_command(command)
    return get_test_results(expected_output_filename, output_filename, update_snapshot, strip_regex, strip_keys, sort, use_expected_output)


def run_triggered_background_test(background_test_command, trigger_command, update_snapshot=False, strip_regex=None, strip_keys=None, sort=None, use_expected_output=True):
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    output_filename = script_path(f"{name}-answer.{data_file_type}")
    expected_output_filename = script_path(f"{name}-expected.{data_file_type}")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    background_test = start_command(background_test_command)
    run_command(trigger_command)
    background_test.wait()
    return get_test_results(expected_output_filename, output_filename, update_snapshot, strip_regex, strip_keys, sort, use_expected_output)


def update_file_content(content, filename):
    with open(filename, "wt") as f:
        f.write(content)


def maybe_update_snapshot(src_filename, dst_filename, update_snapshot):
    if not should_update_snapshot(update_snapshot):
        return
    _, ext = os.path.splitext(src_filename)
    copyfile(src_filename, dst_filename)


def should_update_snapshot(update_snapshot):
    print("should_update_snapshot", update_snapshot)
    if update_snapshot:
        return True
    try:
        return os.environ["UPDATE_SNAPSHOT"]
    except KeyError:
        return False


###########################################################
#
# Tools for Mobile Testing
#
###########################################################

ANDROID_BASE_CAPS = {
    'automationName': 'UIAutomator2',
    'platformName': 'Android',
    'platformVersion': os.getenv('ANDROID_PLATFORM_VERSION') or '10',
    'deviceName': os.getenv('ANDROID_DEVICE_VERSION') or 'Android Emulator',
}

IOS_BASE_CAPS = {
    'automationName': 'xcuitest',
    'platformName': 'iOS',
    'platformVersion': os.getenv('IOS_PLATFORM_VERSION') or '13.3',
    'deviceName': os.getenv('IOS_DEVICE_NAME') or 'iPhone 8',
    # 'showIOSLog': False,
}

EXECUTOR = 'http://127.0.0.1:4723/wd/hub'


def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)


def take_screenshot_and_logcat(driver, device_logger, calling_request):
    __save_log_type(driver, device_logger, calling_request, 'logcat')


def take_screenshot_and_syslog(driver, device_logger, calling_request):
    __save_log_type(driver, device_logger, calling_request, 'syslog')


def __save_log_type(driver, device_logger, calling_request, log_type):
    logcat_dir = device_logger.logcat_dir
    screenshot_dir = device_logger.screenshot_dir

    try:
        driver.save_screenshot(os.path.join(screenshot_dir, calling_request + '.png'))
        logcat_data = driver.get_log(log_type)
    except InvalidSessionIdException:
        logcat_data = ''

    with open(os.path.join(logcat_dir, '{}_{}.log'.format(calling_request, log_type)), 'w') as logcat_file:
        for data in logcat_data:
            data_string = '%s:  %s\n' % (data['timestamp'], data['message'].encode('utf-8'))
            logcat_file.write(data_string)


def configure_logging_and_screenshots(config):
    if not hasattr(config, 'input'):
        base_path = script_path("")
        current_day = '{:%Y-%m-%d-%H%M}'.format(datetime.datetime.now())
        ensure_dir(os.path.join(base_path, 'input', current_day))
        result_dir = os.path.join(base_path, 'results', current_day)
        ensure_dir(result_dir)
        result_dir_test_run = result_dir
        ensure_dir(os.path.join(result_dir_test_run, 'screenshots'))
        ensure_dir(os.path.join(result_dir_test_run, 'logcat'))
        config.screen_shot_dir = os.path.join(result_dir_test_run, 'screenshots')
        config.logcat_dir = os.path.join(result_dir_test_run, 'logcat')


class DeviceLogger:
    def __init__(self, logcat_dir, screenshot_dir):
        self.screenshot_dir = screenshot_dir
        self.logcat_dir = logcat_dir


@pytest.fixture(scope='function')
def device_logger(request):
    logcat_dir = request.config.logcat_dir
    screenshot_dir = request.config.screen_shot_dir
    return DeviceLogger(logcat_dir, screenshot_dir)


@pytest.fixture(scope='function')
def driver_android(request, device_logger):
    calling_request = request._pyfuncitem.name

    caps = copy.copy(ANDROID_BASE_CAPS)
    caps['name'] = calling_request
    caps['app'] = request.config.ANDROID_APP
    caps['appActivity'] = request.config.ANDROID_APP_ACTIVITY

    driver = webdriver.Remote(
        command_executor=EXECUTOR,
        desired_capabilities=caps
    )

    def fin():
        take_screenshot_and_logcat(driver, device_logger, calling_request)
        driver.quit()

    request.addfinalizer(fin)

    driver.implicitly_wait(10)
    return driver


@pytest.fixture(scope='function')
def driver_ios(request, device_logger):
    calling_request = request._pyfuncitem.name

    caps = copy.copy(IOS_BASE_CAPS)
    caps['name'] = calling_request
    caps['app'] = request.config.IOS_APP

    driver = webdriver.Remote(
        command_executor=EXECUTOR,
        desired_capabilities=caps
    )

    def fin():
        take_screenshot_and_syslog(driver, device_logger, calling_request)
        driver.quit()

    request.addfinalizer(fin)

    driver.implicitly_wait(10)
    return driver
