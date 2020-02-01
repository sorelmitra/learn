import os

from verifit import configure_logging_and_screenshots

def pytest_configure(config):
	configure_logging_and_screenshots(config)

	config.ANDROID_APP = os.path.abspath('/Users/sorel/1data/w/github/other/appium/sample-code/apps/ApiDemos-debug.apk')
	config.ANDROID_APP_ACTIVITY = '.app.SearchInvoke'

	config.IOS_APP = os.path.abspath('/Users/sorel/1data/w/github/other/appium/sample-code/apps/TestApp.app.zip')
