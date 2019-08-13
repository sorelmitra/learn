Feature: Basic Chat API

Scenario: Post: Delete All
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Post and Notify: New Message
	Given Chat API Host "http://localhost:8201"

	When WebSocket "1" Connect to Server "ws://localhost:8201/notifications/v1/"
	When WebSocket "1" Send "notify_register_1.json"
	Then WebSocket "1" Get response like "notification-registration.*id.*: (\d+).*chatapi-systest.*success.*true"
	When WebSocket "2" Connect to Server "ws://localhost:8201/notifications/v1/"
	When WebSocket "2" Send "notify_register_2.json"
	Then WebSocket "2" Get response like "notification-registration.*id.*: (\d+).*chatapi-systest.*success.*true"

	When "POST" "message_1.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*: (\d+).*success.*true"

	Then WebSocket "1" Get response like "body.*Message 1.*id.*: (\d+).*success.*true"
	Then WebSocket "2" Get response like "body.*Message 1.*id.*: (\d+).*success.*true"

Scenario: Post: Get by ID
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1/$3"
	Then Status is "200" and response contains "body.*Message 1.*id.*$3+.*success.*true"

Scenario: Post: List All
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*\d+.*success.*true"

Scenario: Post: Delete All
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Notify: Incomplete Registration
	When WebSocket "1" Connect to Server "ws://localhost:8201/notifications/v1/"
	When Sleep "3" seconds
	When WebSocket "1" Send "notify_register_1.json"
	Then WebSocket "1" Get response like "chatapi-systest.*could not find entry.*success.*false"

Scenario: Notify: Bad Command
	When WebSocket "1" Connect to Server "ws://localhost:8201/notifications/v1/"
	When WebSocket "1" Send "notify_bad_command.json"
	Then WebSocket "1" Get response like "chatapi-systest.*command not understood.*success.*false"
