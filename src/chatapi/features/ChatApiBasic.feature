Feature: CHAT_API_BASIC: Basic Chat API

Scenario: CHAT_API_BASIC_HELLO: Hello, World Chat API
	Given Browse to URL "https://botagg-239511.appspot.com/chat/"
	Then Check "/html/body" element contains "hi world"
