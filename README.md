# botagg

# Introduction

This is a PoC project for putting together a chat bot from open-source libraries.

# Goals

Build a PoC set of applications that offer a web chat platform where visitors can discuss with a chat bot and where chat bot owners can administer their bots.

This platform will have 3 apps:

* **Chat Bot Administration App**. This is a web app where chat bot owners can create accounts and configure one of the supported bots, including: connection to one of the supported channels, intent recognition and responses
* **Chat Bot App**. This is a chat bot RESTful API that can be configured from the chat bot admin app and called from anywhere
* **Web Chat App**. This is a web/mobile app where the visitors login and talk to the bot

Additionally, this platform will conform to the following constraints (initially it is enough if we at least *design* for it):

* **Scalability**. Ease of enlarging or downsizing the setup as the load indicates
* **High Availability**. The system is still available if portions of it go down
* **Flexible Deploy**. The system can be deployed on prem as well as in a cloud environment
* **Cloud-Vendor Independence**. The cloud deploying can be done on any cloud vendor with minimal additional development
* **New Bots**. The platform should support adding new bots, whether via internal development or external
* **New Visitor Channels**. The platform should support adding new visitor channels
* **Open to Contact Centers**. The platform should support future additions of Contact Centers of any kind

# Team, Tools, and Process

## Process

This will be an Agile project.
Agile variant TBD.

## Team

- Product Owner: Sorel Mitra
- Scrum Master or Agile PM: Sorel Mitra
- Development Team: Sorel Mitra
- Business Analyst: Sorel Mitra
- Customer: Not Applicable, as this is a PoC only

## Tools

This project will demonstrate software development with the following tools and technologies:

- **Source Control**: GIT with GitHub
- **Issue & Sprint Tracking**: GitHub Issues
- **Stories Definition & Acceptance Criteria**: Gherkin Language
- **Automatic Acceptance Tests**: Python with Behave/Gherkin
- **Web Domain**: <span style="color:red">TBD, free?</span>
- **Web Hosting**: <span style="color:red">AWS?</span>
- **Architecture**: Microservices with Docker and Kubernetes, Cloud/On Premise
- **Cloud Hosting**: AWS
- **Chat Bot Administration App**: 
	- **Backend**: Node.js with https://expressjs.com/
	- **Frontend**: React JS https://reactjs.org/
- **Chat Bot App**: Node.js with https://expressjs.com/, REST, and https://botlang.org
- **Web Chat App**:
	- **Frontend**: React Native https://facebook.github.io/react-native/ for mobile
	- **Backend**: Python with https://www.djangoproject.com/
