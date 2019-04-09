Feature: WC_BASIC: Basic Web Chat Capabilities

Scenario: WC_BASIC_HELLO: As a Product Owner I want to See a Hello, World Web Chat
	Given Browse to URL "https://botagg.appspot.com/webchat/"
	And Input "Hello" to "wcInputText"
	When Click "wcButtonSend"
	Then Check field "wcOutputConversation" receives "Hello" in "1 second"
	Then Check field "wcOutputConversation" receives "Hello, this is TBDBot talking to you. How may I help you?" in "3 seconds"
