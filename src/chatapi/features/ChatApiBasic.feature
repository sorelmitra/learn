Feature: Basic Chat API

Scenario: Delete All Posts
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Post Message and Notify
	Given Chat API Host "http://localhost:8201"
	When WebSocket Connect to Server "ws://localhost:8201/notifications/v1"
	When WebSocket Send "register_for_notifications.json"
	Then WebSocket Get response like "notification-registration.*Chat API Tests.*id.*: (\d+).*success.*true"
	When "POST" "message_1.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*: (\d+).*success.*true"
	Then WebSocket Get response like "body.*Message 1.*id.*: (\d+).*success.*true"

Scenario: Get Post by ID
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1/$1"
	Then Status is "200" and response contains "body.*Message 1.*id.*$1+.*success.*true"

Scenario: List All Posts
	Given Chat API Host "http://localhost:8201"
	When "GET" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*\d+.*success.*true"

Scenario: Delete All Posts
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

