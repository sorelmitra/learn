from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located

from login import *
from verifit import *


@pytest.mark.skip(reason="Outdated")
def go_to_your_content(driver, wait):
	# Login
	link_text_content = "Your Content"
	login(driver, wait, link_text_content)

	# Go to Your Content
	driver.find_element_by_link_text(link_text_content).click()


@pytest.mark.skip(reason="Outdated")
def do_add_content(driver, wait):
	# Go to Add Content
	link_text_add_content = "Add Content"
	wait.until(presence_of_element_located((By.LINK_TEXT, link_text_add_content)))
	driver.find_element_by_link_text(link_text_add_content).click()

	# Fill in Add Content form
	link_text_title = "Briz briz"
	driver.find_element_by_name("url").send_keys("http://briz.com")
	driver.find_element_by_name("title").send_keys(link_text_title)
	driver.find_element_by_css_selector(f'form>input[type="submit"][value="{link_text_add_content}"]').click()
	wait.until(presence_of_element_located((By.LINK_TEXT, link_text_title)))


@pytest.mark.skip(reason="Outdated")
def do_update_content(driver, wait):
	# Go to Update Content
	link_text_title = "Briz briz"
	wait.until(presence_of_element_located((By.LINK_TEXT, link_text_title)))
	driver.find_element_by_link_text(link_text_title).click()

	# Fill in Update Content form
	link_text_title = "Briz briz 2"
	url = driver.find_element_by_name("url")
	url.clear()
	url.send_keys("http://briz2.com")
	title = driver.find_element_by_name("title")
	title.clear()
	title.send_keys(link_text_title)
	driver.find_element_by_css_selector(f'form>input[type="submit"][value="Update Content"]').click()
	wait.until(presence_of_element_located((By.LINK_TEXT, link_text_title)))


@pytest.mark.skip(reason="Outdated")
def test_content():
	with webdriver.Chrome() as driver:
		wait = WebDriverWait(driver, 10)

		go_to_your_content(driver, wait)
		do_add_content(driver, wait)
		do_update_content(driver, wait)


@pytest.mark.skip(reason="Outdated")
def test_content_wrong():
	with webdriver.Chrome() as driver:
		wait = WebDriverWait(driver, 10)

		# Login
		login(driver, wait, "Your Content")

		# Go to Your Content - intentional fail
		driver.find_element_by_link_text("Your Not Content Ha Ha").click()
