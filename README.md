# botagg

# Overview

This is a demonstrative project that aims to build a ChatBot Aggregator app with mobile front-end and web admin app, as a way to show how I assure product quality while developing with new technologies.


# Goals

Build a demo app that offers a mobile chat platform where visitors can discuss with a chat bot and a web app where chat bot owners can administer their bots

This platform will have 5 components:

* **SampleBot Service**. This is a Web Service that implements a simple bot. Functions TBD.
* **SampleBot Admin**. This is a web app where chat bot owners can create accounts and configure SampleBot dialogs
* **ChatBot API**. This is a chat bot API that serves as an interface between **Chat API** and various bots (for now only SampleBot). It acts both as a client and as posts observer for the **Chat API**. When a message is posted to **Chat API**, the ChatBot API takes it and forwards it to the **bot**. When the **bot** responds, the ChatBot API posts the bot response back to **Chat API**
* **Chat API**. This is a chat API that allows posting messages and getting notifications when messages are posted
* **Chat Mobile**. This is a mobile app where the visitors can write messages and read messages posted by other chat members

Additionally, this platform will conform to the following constraints (initially it is enough if we at least *design* for it):

* **Scalability**. Ease of enlarging or downsizing the setup as the load indicates
* **High Availability**. The system is still available if portions of it go down
* **Flexible Deploy**. The system can be deployed on premise as well as in a cloud environment
* **Cloud-Vendor Independence**. The cloud deploying can be done on any cloud vendor with minimal additional development
* **New Bots**. The platform should support adding new bots, whether via internal development or external
* **New Caller Channels**. The platform should support adding new channels for the entities that come to talk to the bots (named "Caller" here)
* **Open to Contact Centers**. The platform should support future additions of Contact Centers of any kind

# Team, Tools, and Process

## Process

This will be an Agile project.

## Team

- Product Owner: Sorel Mitra
- Scrum Master or Agile PM: Sorel Mitra
- Development Team: Sorel Mitra
- Business Analyst: Sorel Mitra
- Customer: Not Applicable, as this is a demo only

## Tools

This project demonstrates software development with the following tools and technologies:

- **Source Control**: GIT with GitHub
- **Issue & Sprint Tracking**: GitHub Issues and Project/Board
- **Stories Definition & Acceptance Criteria**: Cucumber and Detox
- **Automatic Acceptance Tests**: Cucumber-JS with Selenium WebDriver for Web, Detox/Appium for Mobile
- **Web Domain**: <span style="color:red">None for now. Free Domains are not an option as they are associated with scams and bad practices</span>
- **Web Hosting**: <span style="color:red">Google Cloud</span>
- **Architecture**: Microservices with Docker and Kubernetes for On Premise, Google Free-Tier technologies for Cloud (Docker is [not supported](https://cloud.google.com/cloud-build/docs/quickstart-docker) in Google Free Tier)
- **Cloud Hosting**: Google Cloud
- **Components**:
	* **SampleBot Service**: [Node.js](https://nodejs.org) with [Express JS](https://expressjs.com/), [BotML](https://github.com/codename-co/botml), WebSockets (bot communications), REST (admin)
	* **SampleBot Admin**: [React JS](https://reactjs.org/)
	* **ChatBot API**: Node.js, WebSockets
	* **Chat API**: Python with [Django](https://www.djangoproject.com/), REST, WebSockets
	* **Chat Mobile**. [React Native](https://facebook.github.io/react-native/)

# Documentation

## Solution & Architecture

- Solution & Architecture [Document](https://github.com/sorelmitra/botagg/blob/master/docs/SolutionArchitecture.md)

## User & API Guides

- User Guide for [Chat Mobile](https://github.com/sorelmitra/botagg/blob/master/docs/ChatMobileUserGuide.md)

- User Guide for [Chat API](https://github.com/sorelmitra/botagg/blob/master/docs/ChatAPIGuide.md)

- User Guide for [Sample Bot](https://github.com/sorelmitra/botagg/blob/master/docs/SampleBotGuide.md)

## Development

- General [Development Guide](https://github.com/sorelmitra/botagg/blob/master/docs/DevelopmentGuide.md)

- Setting up [Postgres in Docker with Certificates](https://github.com/sorelmitra/botagg/blob/master/docs/PostgresDockerCertificates.md)
