Feature: Basic Chat API

Scenario: Delete All Posts
	Given Chat API Host "http://localhost:8201"
	When "DELETE" "empty.json" to "/posts/v1"
	Then Status is "200" and response contains "reason.*all messages deleted.*success.*true"

Scenario: Post Message and Notify
	Given Chat API Host "http://localhost:8201"
	#When "POST" "register_posts.json" to "/"
	When "POST" "message_1.json" to "/posts/v1"
	Then Status is "200" and response contains "body.*Message 1.*id.*: (\d+).*success.*true"
	#Then 

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

