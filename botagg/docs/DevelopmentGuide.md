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

	```bash
	pyenv global 3.7.2 2.7.15
	```

3. Install tools for automatic tests: Behave, Requests:

	```bash
	pip install behave
	pip install requests
	pip install colorama
	```

4. Install [Docker Machine](https://docs.docker.com/machine/install-machine/) and create and start a machine

5. Build the [Postgres with Certificates image](https://github.com/sorelmitra/botagg/blob/master/docs/PostgresDockerCertificates.md#7-create-and-build-a-docker-postgres-image) and [install the certificates](https://github.com/sorelmitra/botagg/blob/master/docs/PostgresDockerCertificates.md#11-copy-client-certificates)

### For Chat Mobile App

1. Install [Node.js](https://nodejs.org)

2. Install an XCode version that is supported by Apple

3. [Optional] Install [Appium](http://appium.io/docs/en/about-appium/getting-started/) with [XCUITest Driver](http://appium.io/docs/en/drivers/ios-xcuitest/index.html)

	This step is not required, as Appium tests are deprecated on this project. I left them in the source code as an example. If you do install Appium, then you'll be able to run the Mobile Cucumber tests, but please mind that they're just an example. The automated tests are based on Detox.

## Clone the Repository

```bash
git clone https://github.com/sorelmitra/botagg.git
```

## Start Chat API

### Prepare Chat API for Running

CD to the Chat API source directory:

```bash
cd src/chatapi
```

Create an isolated Python environment, and install dependencies:

```bash
virtualenv pyvirtenv
npm run env-install
```

Create the `postgres-cert` docker image:

```bash
cd src/postgres
make
```

Start the local Postgres with Certificates:

```bash
cd src/docker
docker-compose up -d
```

Check that Postgres is up and running:

```bash
psql "sslmode=verify-ca host=192.168.99.100 port=5203" -U postgres
```

Create the database in the local Postgres:

```sql
create database chatapi;
```

CD back to the Chat API source directory:

```bash
cd src/chatapi
```

Create a super user for your Django app

```bash
npm run createsuperuser
```

### Configuration

Create a file `chatapisite/settings.py` with the following content:

```python
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
```

### Start Chat API On a Local PC

Run the Django migrations to set up your models:

```bash
npm run makemigrations
npm run migrate
```

Start a local web server:

```bash
cd src/chatapi
npm start-dev-local
```

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

	```bash
	python manage.py collectstatic
	gcloud app deploy
	```

### Test Chat API in the Cloud

Test it in the same way as locally, but use http://botagg-239511.appspot.com/chat/ instead of the localhost URL.

## Start Chat Mobile App

### Broken!

2019-12-12: The app is broken after I've upgraded to Mac OS Catalina and Xcode 11. I tried updating to React 0.61.5 but I can't build.

I don't have enough time to try and fix it but probably I'd better copy my source code elsewhere and start over with a new React Latest Version project.

Thoughts: It's not nice to find this project after 6 months and discover I can't run it anymore. The pace with React Native might be too fast for my limited time. I believe that had I had an Xcode Objective-C or Swift project I wouldn't have had it broken in just 6 months.

### Prepare to Run

CD to the Chat Mobile source directory:

```bash
cd src/chatmob
```

Install the dependencies:

```bash
npm install
```

### Configuration

Create a `.env` file with the following content:

```bash
CHAT_NAME=botagg-chatmob
CHAT_POSTS_URL=http://localhost:8201/posts/v1/
CHAT_NOTIFICATIONS_URL=ws://localhost:8201/notifications/v1/
```

### Start React Native Server

To start it and connect to the Chat API specified in `.env`:

```bash
npm start
```

If you get this error `Loading dependency graph...Failed to construct transformer: Error: Option 'mapper' isn't supported by the Node crawler`, do this:

1. Install `patch-package`

		npm install --global patch-package

2. Make sure you have `jest-haste` at least `24.8.0`

3. CD `src/chatmob`

4. Run this:

		npx patch-package

	It will automatically find `./patches/jest-haste-map+24.8.0.patch` and apply it.

5. Run again `npm start`. The error is gone.

### Start the iOS App in the Simulator

Run:

```bash
npm run ios
```

### Test the Chat Mobile App

Run the automated tests:

```bash
npm run test
```

## Start the Sample Bot

### Configuration

CD to the source directory

```bash
cd src/samplebot
```

Create a file `.env` with the following content:

```bash
PORT=8202
```

### Start

Start the service:

```bash
npm start
```

## Start the ChatBot API

### Configuration

CD to the source directory

```bash
cd src/chatbotapi
```

Create a file `.env` with the following content:

```bash
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
```

### Start

Start the service:

```bash
npm start
```

# Development

This is a learning and demo project. Thus, it does not employ all techniques for ensuring quality of a production project. However, I do want to understand the tools that help to ensure quality on the technologies used, so I did employ automated tests.

A few guidelines:

- TDD/BDD is encouraged. For lack of time, I won't be employing all aspects of automated testing. Being forced to choose only one solution, I opted for End-to-End tests because at the end of the day they give you insurance that your entire solution works in the deployed form. I favor BDD with Cucumber, but for Mobile Appium doesn't *behave* like I want (pun intended), so I sticked with Detox/Jest and it's test descriptions with `it('should...')`
- Good code quality is encouraged. It pays off, even if I limit this project for demo purposes only. For once, I learn what it means to maintain code quality with these languages. Secondly, it actually helps when you have little time: a good-quality code means I use very little time to "connect" to the project when I get the time
- Code comments explaining the technologies are encouraged if time permits. This is my standard way of learning a new technology: I create a dummy programming file, in which I add comments documenting the various aspects of programming. For this project, since it actually does something meaningful, there is less need for such comments, but still they are welcome
