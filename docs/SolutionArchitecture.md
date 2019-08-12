# Overview

BotAgg is a cloud/on premise demo solution for some technologies that I learned, also aiming to provide a demo platform where visitors can discuss with a chat bot and a web app where chat bot owners can administer their bots.

This is what is currently implemented:

* **Chat API**. This is a chat RESTful API that allows posting messages
* **Chat Mobile**. This is a mobile app where the visitors can write messages

![Solution](https://github.com/sorelmitra/botagg/blob/master/docs/diagrams/BotAggSolution.png "Bot Agg Solution")

# Scalability and High Availability

In order to achieve Scalability, the services will be modified as follows:

## Chat API

Right now it is designed to offer a REST API for posting messages and store them into a Postgres database. It also pushes notifications for new posts via WebSockets.

This will be changed as such:

TBD, use Kafka as alternative to REST API, too.

* Storing messages in Postgres will be decoupled internally, Chat API will be using the [Strategy](https://en.wikipedia.org/wiki/Strategy_pattern) design pattern to choose how to store messages based on config
	* There will be a Postgres Storing Strategy, for historic reasons (it exists right now, built in Chat API)
	* There will be a Kafka Storing Strategy, for scalability and resilience. It will write to the "Posts" topic in Kafka. This will allow for multiple instances of Chat API to store messages consistently, at the same time
* Notifying for new posts will be similarly decoupled:
	* There will be a Postgres Notification Strategy, for historical reasons. It will take the new message from the database and notify via WebSockets
	* There will be a Kafka Notification Strategy, which will be a consumer for the "Posts" topic, taking those messages as they come and notifying for them via WebSockets. Again this will allow for scalability and resilience since any Chat API instance will be able to notify for messages posted by any other instance

# ChatBot API -> Bot Connector

Currently, ChatBot API registers to **Chat API** to get posts notifications. When such a notification arrives, it passes it to its internal BotConnector, which in turn passes it to the corresponding bot.

When the bot responds, the BotConnector passes the message to Chat Bot API main class, which in turn posts the bot response back to **Chat API**.

This will change (and simplify!) as follows:

* The ChatBot API will be renamed to BotConnector
* BotConnector will have a setting specifying what strategy (Strategy Design Pattern) to use to publish messages to a bot. Upon startup, it will instantiate the right Strategy based on this setting
* Each strategy will correspond to a particular bot
* SampleBot will use Kafka to receive messages
* When BotConnector gets a notification from **Chat API**, it will pass it to the strategy instance it has
* There will be a Kafka Publishing Strategy, which will store the visitor message in Kafka. This will allow multiple instances of ChatBot API to publish messages to a bot
* When a bot responds, the message will come into the strategy instance. BotConnector will take that message and post it to **Chat API**

# SampleBot

The Sample Bot will be modified to use Kafka as a way for receiving messages and for posting responses back.

It will support multiple instances. For the sake of it.
