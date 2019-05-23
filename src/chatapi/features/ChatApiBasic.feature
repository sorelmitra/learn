Feature: CHAT_API_BASIC: Basic Chat API

Scenario: CHAT_API_BASIC_HELLO: Hello, World Chat API
	Given Chat API Host "http://localhost:8000"
	When "POST" "message_1.json" to "/posts"
	Then Status is "200" and response is "message_1_resp_success.json"
