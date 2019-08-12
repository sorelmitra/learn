# Introduction

This is a short guide to help anyone (including the creator of this project!) to get up-to-speed with developing on this project.

# Overview

The project folder is organized as follows:

* **ROOT**

	This holds the sources of this project. It's a Node.JS project initialized with npm. Currently it doesn't define any action or source except for a docker-compose file for Postgres, but it will hold npm tasks for the on-premise solution.

	* **docs**
	
		Contains documentation for this project, including the current document
	
	* **src**

		This folder contains the actual sources of this project. Each component has its own folder. Additional folders are for the various tools used.

		* **chatapi**

			The source folder for the Chat API. This is a Django project with Python 3. It runs in the Google Cloud or on-premise.

			Although a Django project does not require a Node.JS folder, I've initialized npm in this folder to help me run the tasks more easily.
			
			Because this is a demo project, the Chat API does **not** have its own tests. Instead, it is being tested via the E2E tests ran on the Chat Mobile App.
			
			(Currently, it **does** have a "feature" folder leftover from previous attempts, but I will not delete it as I plan to use it as a template for the SampleBot Admin App, which will have its own E2E tests. Then, I will delete it.)

		* **chatmob**

			Source folder for the Chat Mobile App. This is a React Native CLI application, ejected from Expo.

			It only runs on iOS so far.

			The **e2e** folder holds the End-to-End tests, which are based on Detox+Jest.

		* **postgres**
		
			Contains a Dockerfile for Postgres with Certificates Authentication

# Quick Start

## Install the Development Tools

### For Chat API

1. Install [Python 2 and 3 with pyenv](https://weknowinc.com/blog/running-multiple-python-versions-mac-osx)

2. Make Python 3 the default and Python 2 available

		pyenv global 3.7.2 2.7.15

3. Install tools for automatic tests: Behave, Requests:

		pip install behave
		pip install requests
		pip install colorama

4. Install [Docker Machine](https://docs.docker.com/machine/install-machine/) and create and start a machine

5. Build the [Postgres with Certificates image](https://github.com/sorelmitra/botagg/blob/master/docs/PostgresDockerCertificates.md#7-create-and-build-a-docker-postgres-image) and [install the certificates](https://github.com/sorelmitra/botagg/blob/master/docs/PostgresDockerCertificates.md#11-copy-client-certificates)

### For Chat Mobile App

1. Install [Node.js](https://nodejs.org)

2. Install an XCode version that is supported by Apple

3. [Optional] Install [Appium](http://appium.io/docs/en/about-appium/getting-started/) with [XCUITest Driver](http://appium.io/docs/en/drivers/ios-xcuitest/index.html)

	This step is not required, as Appium tests are deprecated on this project. I left them in the source code as an example. If you do install Appium, then you'll be able to run the Mobile Cucumber tests, but please mind that they're just an example. The automated tests are based on Detox.

## Clone the Repository

	git clone https://github.com/sorelmitra/botagg.git

## Start Chat API

### Prepare Chat API for Running

CD to the Chat API source directory:

	cd src/chatapi

Create an isolated Python environment, and install dependencies:

	virtualenv env
	npm run env-install

Create the `postgres-cert` docker image:

	cd src/postgres
	make

Start the local Postgres with Certificates:

	cd src/docker
	docker-compose up -d

Check that Postgres is up and running:

	psql "sslmode=verify-ca host=192.168.99.100 port=5203" -U postgres

Create the database in the local Postgres:

	create database chatapi;

CD back to the Chat API source directory:

	cd src/chatapi

Create a super user for your Django app

	npm run createsuperuser

### Configuration

Create a file `chatapisite/settings.py` with the following content:

	from .settings_common import *

	# SECURITY WARNING: keep the secret key used in production secret!
	SECRET_KEY = '<YOUR.SECRET.KEY.USED.IN.DJANGO>'

	# SECURITY WARNING: don't run with debug turned on in production!
	DEBUG = <True|False>

	ALLOWED_HOSTS = [
		'<some host>',
		'<some other host>'
	]

	# Database
	# https://docs.djangoproject.com/en/2.2/ref/settings/#databases
	# Use whatever database you need and have a driver for. Below we have a sample for postgres

	DATABASES = {
		'default': {
			'ENGINE': 'django.db.backends.postgresql',
			'NAME': 'chatapi',
			'USER': '<Postgres User>',
			'HOST': '<Postgres Host>',
			'PORT': '<Postgres Port>'
		},
	}


### Start Chat API On a Local PC

Run the Django migrations to set up your models:

	npm run makemigrations
	npm run migrate

Start a local web server:

	cd src/chatapi
	npm start-dev-local

When you no longer need the server, hit `Ctr+C` to stop it.

### Test Chat API Locally

In your web browser, enter this address: http://localhost:8000. You should see the "Hello, World" message.

### Start Chat API in Google Cloud

1. Create a [Google Cloud Project](https://cloud.google.com/resource-manager/docs/creating-managing-projects) in a region close to you, named "botagg-239511"

2. Install the [Google Cloud SDK](https://cloud.google.com/sdk/docs/quickstart-macos)

3. Enable [Cloud SQL Admin API](https://console.cloud.google.com/flows/enableapi?apiid=sqladmin.googleapis.com&redirect=https:%2F%2Fcloud.google.com%2Fpython%2Fdjango%2Fappengine&showconfirmation=true&_ga=2.133304654.-1155677675.1557927494)

4. Make sure "env" is added to .gcloudignore

5. Prepare the Cloud Postgres:

	TBD

6. Deploy the app to the cloud:

		python manage.py collectstatic
		gcloud app deploy

### Test Chat API in the Cloud

Test it in the same way as locally, but use http://botagg-239511.appspot.com/chat/ instead of the localhost URL.

## Start Chat Mobile App

### Prepare to Run

CD to the Chat Mobile source directory:

	cd src/chatmob

Install the dependencies:

	npm install

### Configuration

Create a `.env` file with the following content:

	CHAT_NAME=botagg-chatmob
	CHAT_POSTS_URL=http://localhost:8201/posts/v1/
	CHAT_NOTIFICATIONS_URL=ws://localhost:8201/notifications/v1/

### Start React Native Server

To start it set-up to connect to the Chat API running on the Local PC:

	npm start-dev-local

To start it set-up to connect to the Chat API running in the Cloud:

	npm start-dev-cloud

### Start the iOS App in the Simulator

Run:

	npm run ios

### Test the Chat Mobile App

Run the automated tests:

	npm run test

## Start the Sample Bot

### Configuration

CD to the source directory

	cd src/samplebot

Create a file `.env` with the following content:

	PORT=8202

### Start

Start the service:

	npm start

## Start the ChatBot API

### One-time Setup

Install [Kafka](https://kafka.apache.org) on your PC and add it's `bin/` directory to your `PATH`.

CD to the Kafka config source:

	cd src/kafka

Create the `zookeeper.properties` file with the following content:

	# the directory where the snapshot is stored.
	dataDir=/tmp/zookeeper
	# the port at which the clients will connect
	clientPort=2181
	# disable the per-ip limit on the number of connections since this is a non-production config
	maxClientCnxns=0

Create the `server.properties` file with the following content:

	# see kafka.server.KafkaConfig for additional details and defaults

	############################# Server Basics #############################

	# The id of the broker. This must be set to a unique integer for each broker.
	broker.id=0

	############################# Socket Server Settings #############################

	# The address the socket server listens on. It will get the value returned from 
	# java.net.InetAddress.getCanonicalHostName() if not configured.
	#   FORMAT:
	#     listeners = listener_name://host_name:port
	#   EXAMPLE:
	#     listeners = PLAINTEXT://your.host.name:9092
	listeners=PLAINTEXT://127.0.0.1:9092

	# Hostname and port the broker will advertise to producers and consumers. If not set, 
	# it uses the value for "listeners" if configured.  Otherwise, it will use the value
	# returned from java.net.InetAddress.getCanonicalHostName().
	#advertised.listeners=PLAINTEXT://your.host.name:9092

	# Maps listener names to security protocols, the default is for them to be the same. See the config documentation for more details
	#listener.security.protocol.map=PLAINTEXT:PLAINTEXT,SSL:SSL,SASL_PLAINTEXT:SASL_PLAINTEXT,SASL_SSL:SASL_SSL

	# The number of threads that the server uses for receiving requests from the network and sending responses to the network
	num.network.threads=3

	# The number of threads that the server uses for processing requests, which may include disk I/O
	num.io.threads=8

	# The send buffer (SO_SNDBUF) used by the socket server
	socket.send.buffer.bytes=102400

	# The receive buffer (SO_RCVBUF) used by the socket server
	socket.receive.buffer.bytes=102400

	# The maximum size of a request that the socket server will accept (protection against OOM)
	socket.request.max.bytes=104857600


	############################# Log Basics #############################

	# A comma separated list of directories under which to store log files
	log.dirs=/tmp/kafka-logs

	# The default number of log partitions per topic. More partitions allow greater
	# parallelism for consumption, but this will also result in more files across
	# the brokers.
	num.partitions=1

	# The number of threads per data directory to be used for log recovery at startup and flushing at shutdown.
	# This value is recommended to be increased for installations with data dirs located in RAID array.
	num.recovery.threads.per.data.dir=1

	############################# Internal Topic Settings  #############################
	# The replication factor for the group metadata internal topics "__consumer_offsets" and "__transaction_state"
	# For anything other than development testing, a value greater than 1 is recommended for to ensure availability such as 3.
	offsets.topic.replication.factor=1
	transaction.state.log.replication.factor=1
	transaction.state.log.min.isr=1

	############################# Log Flush Policy #############################

	# Messages are immediately written to the filesystem but by default we only fsync() to sync
	# the OS cache lazily. The following configurations control the flush of data to disk.
	# There are a few important trade-offs here:
	#    1. Durability: Unflushed data may be lost if you are not using replication.
	#    2. Latency: Very large flush intervals may lead to latency spikes when the flush does occur as there will be a lot of data to flush.
	#    3. Throughput: The flush is generally the most expensive operation, and a small flush interval may lead to excessive seeks.
	# The settings below allow one to configure the flush policy to flush data after a period of time or
	# every N messages (or both). This can be done globally and overridden on a per-topic basis.

	# The number of messages to accept before forcing a flush of data to disk
	#log.flush.interval.messages=10000

	# The maximum amount of time a message can sit in a log before we force a flush
	#log.flush.interval.ms=1000

	############################# Log Retention Policy #############################

	# The following configurations control the disposal of log segments. The policy can
	# be set to delete segments after a period of time, or after a given size has accumulated.
	# A segment will be deleted whenever *either* of these criteria are met. Deletion always happens
	# from the end of the log.

	# The minimum age of a log file to be eligible for deletion due to age
	log.retention.hours=168

	# A size-based retention policy for logs. Segments are pruned from the log unless the remaining
	# segments drop below log.retention.bytes. Functions independently of log.retention.hours.
	#log.retention.bytes=1073741824

	# The maximum size of a log segment file. When this size is reached a new log segment will be created.
	log.segment.bytes=1073741824

	# The interval at which log segments are checked to see if they can be deleted according
	# to the retention policies
	log.retention.check.interval.ms=300000

	############################# Zookeeper #############################

	# Zookeeper connection string (see zookeeper docs for details).
	# This is a comma separated host:port pairs, each corresponding to a zk
	# server. e.g. "127.0.0.1:3000,127.0.0.1:3001,127.0.0.1:3002".
	# You can also append an optional chroot string to the urls to specify the
	# root directory for all kafka znodes.
	zookeeper.connect=localhost:2181

	# Timeout in ms for connecting to zookeeper
	zookeeper.connection.timeout.ms=6000


	############################# Group Coordinator Settings #############################

	# The following configuration specifies the time, in milliseconds, that the GroupCoordinator will delay the initial consumer rebalance.
	# The rebalance will be further delayed by the value of group.initial.rebalance.delay.ms as new members join the group, up to a maximum of max.poll.interval.ms.
	# The default value for this is 3 seconds.
	# We override this to 0 here as it makes for a better out-of-the-box experience for development and testing.
	# However, in production environments the default value of 3 seconds is more suitable as this will help to avoid unnecessary, and potentially expensive, rebalances during application startup.
	group.initial.rebalance.delay.ms=0

### Start Kafka

Open a new terminal and type:

	cd src/kafka
	zookeeper-server-start.sh zookeeper.properties

Open a second terminal and type:

	cd src/kafka
	kafka-server-start.sh server.properties


### Configuration

CD to the source directory

	cd src/chatbotapi

Create a file `.env` with the following content:

	CHAT_NAME=botagg-chatbot-api
	CHAT_POSTS_URL=http://localhost:8201/posts/v1/
	CHAT_NOTIFICATIONS_URL=ws://localhost:8201/notifications/v1/

	BOT_CONNECTOR_INSTANCE=SAMPLE
	NAME_FOR_BOT=botagg

	BOT_CONNECTOR_DUMMY_TYPE=javascript-class
	BOT_CONNECTOR_DUMMY_PROTOCOL_HANDLER=none
	BOT_CONNECTOR_DUMMY_URI=file://./botconnectors/BotConnectorEcho

	BOT_CONNECTOR_SAMPLE_TYPE=websockets
	BOT_CONNECTOR_SAMPLE_PROTOCOL_HANDLER=file://./botprotocols/BotProtocolSample
	BOT_CONNECTOR_SAMPLE_URI=ws://localhost:8202/samplebot/v1/

### Start

Start the service:

	npm start

# Development

This is a learning and demo project. Thus, it does not employ all techniques for ensuring quality of a production project. However, I do want to understand the tools that help to ensure quality on the technologies used, so I did employ automated tests.

A few guidelines:

- TDD/BDD is encouraged. For lack of time, I won't be employing all aspects of automated testing. Being forced to choose only one solution, I opted for End-to-End tests because at the end of the day they give you insurance that your entire solution works in the deployed form. I favor BDD with Cucumber, but for Mobile Appium doesn't *behave* like I want (pun intended), so I sticked with Detox/Jest and it's test descriptions with `it('should...')`
- Good code quality is encouraged. It pays off, even if I limit this project for demo purposes only. For once, I learn what it means to maintain code quality with these languages. Secondly, it actually helps when you have little time: a good-quality code means I use very little time to "connect" to the project when I get the time
- Code comments explaining the technologies are encouraged if time permits. This is my standard way of learning a new technology: I create a dummy programming file, in which I add comments documenting the various aspects of programming. For this project, since it actually does something meaningful, there is less need for such comments, but still they are welcome
