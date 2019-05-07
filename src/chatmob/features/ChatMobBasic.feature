Feature: CHAT_MOB_BASIC: Basic Mobile Chat Capabilities

Scenario: CHAT_MOB_ADD_MESSAGE: Add message to conversation
	Given Is visible "message input field"
	And There is text "Your message goes here" in "message input field"
	When Tap "message input field"
	And Type "Hello, World!" into "message input field"
	And Type ENTER into "message input field"
	Then There is text "Your message goes here" in "message input field"
	And Is there an element with id "Hello, World!"
