from verifit import *


class TestIOSBasicInteractions():

	def test_should_send_keys_to_inputs(self, driver_ios):
		text_field_el = driver_ios.find_element_by_id('TextField1')
		assert text_field_el.get_attribute('value') is None
		text_field_el.send_keys('Hello World!')
		assert 'Hello World!' == text_field_el.get_attribute('value')

	def test_should_click_a_button_that_opens_an_alert(self, driver_ios):
		button_element_id = 'show alert'
		button_element = driver_ios.find_element_by_accessibility_id(button_element_id)
		button_element.click()

		alert_title_element_id = 'Cool title'
		alert_title_element = driver_ios.find_element_by_accessibility_id(alert_title_element_id)
		alert_title = alert_title_element.get_attribute('name')
		assert 'Cool title' == alert_title
