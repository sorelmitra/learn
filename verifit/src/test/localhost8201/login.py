from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located

def login(driver, wait, link_to_check):
	driver.get("http://localhost:8201")
	driver.find_element_by_link_text("Login").click()
	wait.until(presence_of_element_located((By.NAME, "username")))
	driver.find_element_by_name("username").send_keys("admin@admin.com")
	driver.find_element_by_name("password").send_keys("admin")
	driver.find_element_by_css_selector('form>input[type="submit"]').click()
	wait.until(presence_of_element_located((By.LINK_TEXT, link_to_check
)))

