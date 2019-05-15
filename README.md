# botagg

# Introduction

This is a demo chatbot app with mobile frontend and web admin app.


# Goals

Build a demo app that offers a mobile chat platform where visitors can discuss with a chat bot and a web app where chat bot owners can administer their bots.

This platform will have 5 components:

* **SampleBot Service**. This is a Web Service that implements a simple bot. Functions TBD.
* **SampleBot Admin**. This is a web app where chat bot owners can create accounts and configure SampleBot, including: connection to one of the supported communication channels, intent recognition and responses
* **ChatBot API**. This is a chat bot RESTful API that serves as an interface for talking to various bots (for now only SampleBot). It allows sending a message to a bot and getting the bot response. It registers as an observer for message posting to the **Chat API**
* **Chat API**. This is a chat RESTful API that allows posting messages and getting notifications when messages are posted
* **Chat Mobile**. This is a mobile app where the visitors can write messages and read messages posted by other chat members

Additionally, this platform will conform to the following constraints (initially it is enough if we at least *design* for it):

* **Scalability**. Ease of enlarging or downsizing the setup as the load indicates
* **High Availability**. The system is still available if portions of it go down
* **Flexible Deploy**. The system can be deployed on premise as well as in a cloud environment
* **Cloud-Vendor Independence**. The cloud deploying can be done on any cloud vendor with minimal additional development
* **New Bots**. The platform should support adding new bots, whether via internal development or external
* **New Visitor Channels**. The platform should support adding new visitor channels
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
- **Issue & Sprint Tracking**: GitHub Issues
- **Stories Definition & Acceptance Criteria**: Cucumber and Detox
- **Automatic Acceptance Tests**: Cucumber-JS with Selenium WebDriver for Web, Detox/Appium for Mobile
- **Web Domain**: <span style="color:red">None for now. Free Domains are not an option as they are associated with scams and bad practices</span>
- **Web Hosting**: <span style="color:red">Google Cloud</span>
- **Architecture**: Microservices with Docker and Kubernetes for On Premise, Google Free-Tier technologies for Cloud (Docker is [not supported](https://cloud.google.com/cloud-build/docs/quickstart-docker) in Google Free Tier)
- **Cloud Hosting**: Google Cloud
- **Components**:
	* **SampleBot Service**: Node.js with [Express JS](https://expressjs.com/), REST, and [Botlang](https://botlang.org)
	* **SampleBot Admin**: [React JS](https://reactjs.org/)
	* **ChatBot API**: Node.js, REST
	* **Chat API**: Python with [Django](https://www.djangoproject.com/) and [Thorn](https://pypi.org/project/thorn/)
	* **Chat Mobile**. [React Native](https://facebook.github.io/react-native/)
