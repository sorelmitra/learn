# Introduction

This is a short guide to help anyone (including the creator of this project!) to get up-to-speed with developing on this project.

# Overview

The project folder is organized as follows:

* **ROOT**

	This holds the sources of this project. It's a Node.JS project initialized with npm. Currently it doesn't define any action or source except for a docker-compose file for Postgres, but it will hold npm tasks for the on-premise solution.

	* **docs** - contains documentation for this project, including the current document
	
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

			The 'e2e' folder holds the End-to-End tests, which are based on Detox+Jest.

		* **postgres** - contains a Dockerfile for Postgres with Certificates Authentication

# Quick Start

## Install the Development Tools

### For Chat API

1. Install [Python 2 and 3 with pyenv](https://weknowinc.com/blog/running-multiple-python-versions-mac-osx)

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
	source env/bin/activate
	pip install -r requirements.txt

Run the Django migrations to set up your models:

	python manage.py makemigrations
	python manage.py makemigrations polls
	python manage.py migrate

### Start Chat API On a Local PC

Start a local web server:

	npm start

When you no longer need the server, hit `Ctr+C` to stop it.

### Test Chat API Locally

In your web browser, enter this address: http://localhost:8000. You should see the "Hello, World" message.

### Start Chat API in Google Cloud

1. Create a [Google Cloud Project](https://cloud.google.com/resource-manager/docs/creating-managing-projects) in a region close to you, named "botagg-239511"

2. Install the [Google Cloud SDK](https://cloud.google.com/sdk/docs/quickstart-macos)

3. Enable [Cloud SQL Admin API](https://console.cloud.google.com/flows/enableapi?apiid=sqladmin.googleapis.com&redirect=https:%2F%2Fcloud.google.com%2Fpython%2Fdjango%2Fappengine&showconfirmation=true&_ga=2.133304654.-1155677675.1557927494)

4. Make sure "env" is added to .gcloudignore

5. Deploy the app to the cloud:

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

