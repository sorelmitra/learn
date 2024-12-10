import atexit
import os
import signal
import time
from contextlib import contextmanager
from datetime import datetime

from dotenv import dotenv_values

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException, WebDriverException, \
	ElementClickInterceptedException
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementNotInteractableException


class SiteBase:
	config = {}

	def __init__(self):
		dotenv_path = f".env"
		self.is_closed = False
		self.config = dotenv_values(dotenv_path)
		self.options = Options()
		self.options.add_extension('./dark_reader_extension.crx')
		self.driver = webdriver.Chrome(options=self.options)
		time.sleep(3)
		self.driver.switch_to.window(self.driver.window_handles[0])
		self.wait = WebDriverWait(self.driver, int(self.config['WAIT_SECONDS']))
		atexit.register(self.close)
		signal.signal(signal.SIGTERM, self.close)

	def close(self):
		if self.is_closed:
			return
		time.sleep(3)
		self.driver.close()
		self.driver.quit()
		self.is_closed = True

	def send_action_keys(self, keys, final_delay=0.2):
		actions = ActionChains(self.driver)
		actions.send_keys(keys)
		actions.perform()
		time.sleep(final_delay)

	def upload_file(self, path, file, number=1):
		self.click_element_with_text('Browse files', index=number - 1)
		time.sleep(2)
		self.activate_test_app()
		self.send_system_keys(keys='g', modifiers=["command down", "shift down"])
		self.send_system_keys(path)
		self.send_system_keys(special_keys='return')
		self.send_system_keys(special_keys='right')
		self.send_system_keys(file)
		self.send_system_keys(special_keys='return')

	def send_repeated_key(self, key, count=1, delay=0.1):
		for i in range(0, count):
			self.send_action_keys(key)
			time.sleep(delay)

	def wait_for_element_with_text(self, value):
		self.wait.until(EC.presence_of_element_located((By.XPATH, self.get_xpath_for_element_with_text(value))))

	def wait_for_element_with_attribute_contains(self, name, value):
		self.wait.until(EC.presence_of_element_located((By.XPATH, self.get_xpath_for_element_with_attribute_contains(name, value))))

	def wait_for_element_with_text_visible(self, value):
		self.wait.until(EC.visibility_of_element_located((By.XPATH, self.get_xpath_for_element_with_text(value))))

	def check_element_with_text_visible(self, value, index=0):
		try:
			if index > 0:
				element = self.driver.find_elements_by_xpath(self.get_xpath_for_element_with_text(value))[index]
			else:
				element = self.driver.find_element_by_xpath(self.get_xpath_for_element_with_text(value))
			return element.is_displayed()
		except NoSuchElementException or IndexError:
			return False

	def check_element_with_attribute_contains_visible(self, name, value, index=0):
		try:
			if index > 0:
				element = self.driver.find_elements_by_xpath(self.get_xpath_for_element_with_attribute_contains(name, value))[index]
			else:
				element = self.driver.find_element_by_xpath(self.get_xpath_for_element_with_attribute_contains(name, value))
			return element.is_displayed()
		except NoSuchElementException or IndexError:
			return False

	def wait_for_clickable_element_with_text(self, value):
		self.wait.until(EC.element_to_be_clickable((By.XPATH, self.get_xpath_for_element_with_text(value))))

	def find_element_with_text(self, value, index=0):
		return self.driver.find_elements_by_xpath(self.get_xpath_for_element_with_text(value))[index]

	def find_element_with_attribute_contains(self, name, value, index=0):
		return self.driver.find_elements_by_xpath(self.get_xpath_for_element_with_attribute_contains(name, value))[index]

	def find_sibling_of_element_with_text(self, text, index, sibling_type, sibling_index):
		element = self.find_element_with_text(text, index)
		sibling = self.find_sibling(element, sibling_type, sibling_index)
		return sibling

	def click(self, element):
		try:
			element.click()
		except ElementClickInterceptedException:
			self.driver.execute_script("arguments[0].click()", element)

	def click_element_with_text(self, value, index=0):
		self.wait_for_element_with_text(value)
		element = self.find_element_with_text(value, index)
		self.click(element)

	def click_element_with_attribute_contains(self, name, value, index=0):
		self.wait_for_element_with_attribute_contains(name, value)
		element = self.find_element_with_attribute_contains(name, value, index)
		self.click(element)

	def click_sibling_of_element_with_text(self, text, sibling_type, index=0, sibling_index=0):
		element = self.find_sibling_of_element_with_text(text, index, sibling_type, sibling_index)
		self.click(element)

	def click_descendant_of_element_with_text(self, text, child_type, index=0):
		element = self.find_element_with_text(text, index)
		descendant = element.find_element_by_xpath(f".//{child_type}")
		self.click(descendant)

	def click_descendant_of_sibling_of_element_with_text(self, text, sibling_type, child_type=None, index=0, sibling_index=0):
		sibling = self.find_sibling_of_element_with_text(text, index, sibling_type, sibling_index)
		element = sibling.find_element_by_xpath(f".//{child_type}")
		self.click(element)

	def click_descendant_of_sibling_of_ancestor_of_element_with_text(self, text, sibling_type, child_type=None, ancestor_count=1, index=0, sibling_index=0):
		element = self.find_element_with_text(text, index)
		ancestor = self.find_ancestor(element, ancestor_count)
		sibling = self.find_sibling(ancestor, sibling_type, sibling_index)
		element = sibling.find_element_by_xpath(f".//{child_type}")
		self.click(element)

	def click_ancestor_of_element_with_text(self, text, count, index=0):
		element = self.find_element_with_text(text, index)
		ancestor = self.find_ancestor(element, count)
		self.click(ancestor)

	def show_alert(self, text):
		try:
			self.driver.execute_script(f"alert('{text}');")
		except WebDriverException:
			pass
		time.sleep(15)

	def select_searchable_list_item(self, value, text_to_click, text_that_should_appear=None, text_that_should_become_clickable=None):
		self.click_element_with_text(text_to_click)
		time.sleep(1)
		self.send_action_keys(Keys.TAB)
		self.send_action_keys(value)
		time.sleep(1)
		self.send_action_keys(Keys.DOWN)
		self.send_action_keys(Keys.ENTER)
		self.check_post_conditions(text_that_should_appear, text_that_should_become_clickable)
		self.wait_for_element_with_text(value)

	def select_non_searchable_list_item(self, value, text_that_should_appear=None, text_that_should_become_clickable=None, index=0):
		element = self.find_element_with_text(value, index)
		actions = ActionChains(self.driver)
		actions.move_to_element(element).click()
		actions.perform()
		self.check_post_conditions(text_that_should_appear, text_that_should_become_clickable)

	def select_typeable_list_item(self, item):
		self.send_action_keys(item)
		time.sleep(0.2)
		self.send_repeated_key(Keys.ENTER)

	def select_active_menu_item(self, menu_index):
		pass

	def check_post_conditions(self, text_that_should_appear, text_that_should_become_clickable):
		if text_that_should_appear is not None:
			self.wait_for_element_with_text(text_that_should_appear)
		if text_that_should_become_clickable is not None:
			self.wait_for_clickable_element_with_text(text_that_should_become_clickable)

	def select_radio_box(self, value, radio_values):
		count = radio_values.get(value, 0)
		self.send_repeated_key(Keys.DOWN, count)
		time.sleep(1)
		self.send_action_keys(Keys.SPACE)
		return count

	def sign_on_canvas(self, canvas = None):
		if canvas is None:
			canvas = self.driver.switch_to.active_element
		drawing = ActionChains(self.driver) \
			.click_and_hold(canvas) \
			.move_by_offset(-10, -10) \
			.move_by_offset(20, 0) \
			.move_by_offset(0, 20) \
			.move_by_offset(-20, 0) \
			.move_by_offset(0, -20) \
			.release()
		drawing.perform()

	@staticmethod
	def parse_date_of_birth(loan_input):
		dob = loan_input.get('borrower', {}).get('dob')
		if dob is None:
			return datetime.now().replace(year=1987)
		return datetime.strptime(dob)

	@staticmethod
	def unique(with_precision=False):
		precision = ""
		if with_precision:
			precision = "%H%M%S"
		return datetime.now().strftime(f"%y%m%d{precision}")

	@staticmethod
	def get_xpath_for_element_with_text(value):
		return f"//*[contains(text(), '{value}')]"

	@staticmethod
	def get_xpath_for_element_with_attribute_contains(name, value):
		return f"//a[contains(@{name},'{value}')]"

	@staticmethod
	def find_sibling(element, sibling_type, sibling_index):
		parent = element.find_element_by_xpath("..")
		sibling = parent.find_elements_by_xpath(f"./{sibling_type}")[sibling_index]
		return sibling

	@staticmethod
	def find_ancestor(element, count):
		parent = element
		for i in range(0, count):
			parent = parent.find_element_by_xpath("..")
		return parent

	@contextmanager
	def wait_for_new_window(self, timeout=10):
		handles_before = self.driver.window_handles
		yield
		WebDriverWait(self.driver, timeout).until(
			lambda driver: len(handles_before) != len(driver.window_handles))

	@staticmethod
	def activate_test_app(final_delay=1):
		cmd = "osascript -e 'activate application \"Chrome\"'"
		print(cmd)
		os.system(cmd)
		time.sleep(final_delay)

	@staticmethod
	def send_system_keys(keys=None, special_keys=None, modifiers=None, final_delay=1):
		modifier_string = ''
		keys_string = None
		if keys is not None:
			keys_string = f"\"{keys}\""
		if special_keys is not None:
			keys_string = f"{special_keys}"
		if keys_string is None:
			raise RuntimeError('Missing keys or special_keys!')
		if modifiers is not None:
			n = len(modifiers)
			for i in range(n):
				modifier = modifiers[i]
				modifier_string = modifier_string + modifier
				if i < n - 1:
					modifier_string = modifier_string + ', '
			modifier_string = f" using {{{modifier_string}}}"
		cmd = f"osascript -e 'tell application \"System Events\" to keystroke {keys_string}{modifier_string}'"
		print(cmd)
		os.system(cmd)
		time.sleep(final_delay)

	@staticmethod
	def is_truthy(value):
		return not value.upper() in ['0', 'FALSE', 'NO']


