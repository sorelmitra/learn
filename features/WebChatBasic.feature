Feature: WC_BASIC: Basic Web Chat Capabilities

Scenario: WC_BASIC_HELLO: As a Product Owner I want to See a Hello, World Web Chat
	Given Browse to URL "https://botagg.appspot.com/webchat/"
	Then Check "/html/body" element contains "hi world"
