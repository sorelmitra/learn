Feature: NOT USED, go to e2e/ directory instead!

Feature: CHAT_MOB_BASIC: Basic Mobile Chat Capabilities

Scenario: CHAT_MOB_ADD_MESSAGE: Add message to conversation
	Given Is visible "message input field"
	And There is text "Your message goes here" in "message input field"
	When Tap "message input field"
	And Type "Hello, World!" into "message input field"
	And Type ENTER into "message input field"
	Then There is text "Your message goes here" in "message input field"
	And Is there an element with id "Hello, World!"

Scenario: CHAT_MOB_AUTOSCROLL: Auto scroll conversation when adding messages past the bottom
	Given Is visible "message input field"
	And There is text "Your message goes here" in "message input field"
	When Type "Message 1" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 1"
	When Type "Message 2" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 2"
	When Type "Message 3" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 3"
	When Type "Message 4" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 4"
	When Type "Message 5" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 5"
	When Type "Message 6" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 6"
	When Type "Message 7" into "message input field"
	And Type ENTER into "message input field"
	Then Is there an element with id "Message 7"
	Then Is there an element with id "Hello, World!"
	Then Is visible "Hello, World!"
