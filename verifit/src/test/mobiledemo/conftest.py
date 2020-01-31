import pytest
import datetime
import os
import copy

from helpers import ensure_dir,report_to_sauce, take_screenshot_and_logcat, take_screenshot_and_syslog, IOS_BASE_CAPS, ANDROID_BASE_CAPS, EXECUTOR

from appium import webdriver

def pytest_configure(config):
	if not hasattr(config, 'input'):
		current_day = '{:%Y_%m_%d_%H_%S}'.format(datetime.datetime.now())
		ensure_dir(os.path.join(os.path.dirname(__file__), 'input', current_day))
		result_dir = os.path.join(os.path.dirname(__file__), 'results', current_day)
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
	caps['appActivity'] = request.module.TestAndroidBasicInteractions.APP_ACTIVITY

	driver = webdriver.Remote(
		command_executor=EXECUTOR,
		desired_capabilities=caps
	)

	def fin():
		report_to_sauce(driver.session_id)
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

	driver = webdriver.Remote(
		command_executor=EXECUTOR,
		desired_capabilities=caps
	)

	def fin():
		report_to_sauce(driver.session_id)
		take_screenshot_and_syslog(driver, device_logger, calling_request)
		driver.quit()

	request.addfinalizer(fin)

	driver.implicitly_wait(10)
	return driver

