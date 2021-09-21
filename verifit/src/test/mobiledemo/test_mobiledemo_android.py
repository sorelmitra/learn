import textwrap

from verifit import *


class TestAndroidBasicInteractions():
	PACKAGE = 'io.appium.android.apis'
	ALERT_DIALOG_ACTIVITY = '.app.AlertDialogSamples'

	def test_should_send_keys_to_search_box_and_then_check_the_value(self, driver_android):
		search_box_element = driver_android.find_element_by_id('txt_query_prefill')
		search_box_element.send_keys('Hello world!')

		on_search_requested_button = driver_android.find_element_by_id('btn_start_search')
		on_search_requested_button.click()

		search_text = driver_android.find_element_by_id('android:id/search_src_text')
		search_text_value = search_text.text

		assert 'Hello world!' == search_text_value

	def test_should_click_a_button_that_opens_an_alert_and_then_dismisses_it(self, driver_android):
		driver_android.start_activity(self.PACKAGE, self.ALERT_DIALOG_ACTIVITY)

		open_dialog_button = driver_android.find_element_by_id('io.appium.android.apis:id/two_buttons')
		open_dialog_button.click()

		alert_element = driver_android.find_element_by_id('android:id/alertTitle')
		alert_text = alert_element.text

		assert textwrap.dedent('''\
		Lorem ipsum dolor sit aie consectetur adipiscing
		Plloaso mako nuto siwuf cakso dodtos anr koop.''') == alert_text

		close_dialog_button = driver_android.find_element_by_id('android:id/button1')
		close_dialog_button.click()
