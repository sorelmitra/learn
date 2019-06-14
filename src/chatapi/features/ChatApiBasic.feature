Feature: Basic Chat API

Scenario: Post: Delete All
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Post and Notify: New Message
	Given Chat API Host "http://localhost:8201"
	When WebSocket Connect to Server "ws://localhost:8201/notifications/v1"
	When WebSocket Send "notify_register.json"
	Then WebSocket Get response like "notification-registration.*id.*: (\d+).*chatapi-systest.*success.*true"
	When "POST" "message_1.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*: (\d+).*success.*true"
	#Then WebSocket Get response like "body.*Message 1.*id.*: (\d+).*success.*true"

Scenario: Post: Get by ID
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1/$2"
	Then Status is "200" and response contains "body.*Message 1.*id.*$2+.*success.*true"

Scenario: Post: List All
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*\d+.*success.*true"

Scenario: Post: Delete All
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Notify: Incomplete Registration
	When WebSocket Connect to Server "ws://localhost:8201/notifications/v1"
	When Sleep "3" seconds
	When WebSocket Send "notify_register.json"
	Then WebSocket Get response like "chatapi-systest.*could not find entry.*success.*false"

Scenario: Notify: Bad Command
	When WebSocket Connect to Server "ws://localhost:8201/notifications/v1"
	When WebSocket Send "notify_bad_command.json"
	Then WebSocket Get response like "chatapi-systest.*command not understood.*success.*false"
