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

def get_stack_number():
    global stack_number
    return stack_number

def set_stack_number(number):
    global stack_number
    stack_number = number

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


def load_file_as_string(filepath, format=False):
    with open(filepath) as f:
        content = f.read()
    if format:
        try:
            formatted_content = json.dumps(json.loads(content), indent=2)
            content = formatted_content
        except:
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
    return script_path(f"{name}.json")


def get_output_filename():
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    return script_path(f"{name}-answer.json")


def get_expected_output_filename(name):
    return script_path(f"{name}-expected.json")


def get_test_results(expected_output_filename, output_filename, update_snapshot):
    actual = load_file_as_string(output_filename, format=True)
    update_file_content(actual, output_filename)
    maybe_update_snapshot(output_filename, expected_output_filename, update_snapshot)
    expected = load_file_as_string(expected_output_filename)
    return expected, actual


def run_test(command, update_snapshot=False):
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    output_filename = script_path(f"{name}-answer.json")
    expected_output_filename = script_path(f"{name}-expected.json")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    run_command(command)
    return get_test_results(expected_output_filename, output_filename, update_snapshot)


def run_triggered_background_test(background_test_command, trigger_command, update_snapshot=False):
    global stack_number
    global stack_function_index
    name = inspect.stack()[stack_number][stack_function_index]
    output_filename = script_path(f"{name}-answer.json")
    expected_output_filename = script_path(f"{name}-expected.json")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    background_test = start_command(background_test_command)
    run_command(trigger_command)
    background_test.wait()
    return get_test_results(expected_output_filename, output_filename, update_snapshot)


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
