import copy
import datetime
import inspect
import os
import pytest
import re
import subprocess

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

def script_path(filename):
    # Have tried several ways to get this path, including hardcoded and:
    # dir = sys.path[0] # uses Python Path entries, not reliable
    # This solution is based on reflection.
    # First index is stack function, second index is file name.
    # Partially reliable, works only if
    # - this function is called from within another function from this file,
    #   which in turn is called directly from the test file.
    # Best solution I have so far.
    current_dir = os.path.dirname(inspect.stack()[2][1])
    return os.path.join(current_dir, filename)


def load_file_as_string(filepath):
    with open(filepath) as f:
        content = f.read()
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
    name = inspect.stack()[1][3]
    return script_path(f"{name}.json")


def get_output_filename():
    name = inspect.stack()[1][3]
    return script_path(f"{name}-answer.json")


def get_expected_output_filename(name):
    return script_path(f"{name}-expected.json")


def run_test(command):
    name = inspect.stack()[1][3]
    output_filename = script_path(f"{name}-answer.json")
    expected_output_filename = script_path(f"{name}-expected.json")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    run_command(command)
    got = load_file_as_string(output_filename)
    expected = load_file_as_string(expected_output_filename)
    return expected, got


def run_triggered_background_test(background_test_command, trigger_command):
    name = inspect.stack()[1][3]
    output_filename = script_path(f"{name}-answer.json")
    expected_output_filename = script_path(f"{name}-expected.json")
    try:
        os.unlink(output_filename)
    except FileNotFoundError:
        pass
    background_test = start_command(background_test_command)
    run_command(trigger_command)
    background_test.wait()
    got = load_file_as_string(output_filename)
    expected = load_file_as_string(expected_output_filename)
    return expected, got


def read_token():
    with open("token.txt") as f_token:
        return f_token.read()


def escape_for_json(str1):
    str1 = str1.replace('"', '\\"').replace('\n', '\\n').replace('\\', '\\\\')
    return str1


###########################################################
#
# Tools for GraphQL Testing
#
###########################################################

def prepare_graphql_query(template_filename, query_filename, vars_filename, op_name, output_filename):
    body_filename = template_filename
    with open(script_path(body_filename)) as f_body:
        body = f_body.read()
        with open(script_path(query_filename)) as f_query:
            query = f_query.read()
            with open(script_path(vars_filename)) as f_vars:
                variables = f_vars.read()
    query = escape_for_json(query)
    variables = escape_for_json(variables)
    op_re = re.compile(r'__OPERATION_NAME__')
    body = op_re.sub(op_name, body)
    op_query = re.compile(r'__QUERY__')
    body = op_query.sub(query, body)
    op_query = re.compile(r'__VARIABLES__')
    body = op_query.sub(variables, body)

    with open(output_filename, "wt") as f_out:
        f_out.write(body)

    return output_filename


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
