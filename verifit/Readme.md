# Overview

This project aims to put together a schema that would simplify setting up and using automatic system testing frameworks for several types of projects.

I've named it "verifit" as a contraction of "Verify It (the system)!".



# Introduction

System testing (or application-level testing) is a must for every project. In a big company this is usually done by a dedicated QA team. But what if you're an individual working on a project or, if for any reason, there's no QA in your project?

Automatic system testing comes to rescue!

Usually you can't test a system 100% automatically. But, if you manage to cover the functionality (and perhaps some of the non-functional requirements, such as performance and high-availability - depending on the nature of the project), this will be a big help as the project builds up and you have more and more features to test as you make progress developing it.

Also, on small projects or where you're time constrained, having an automatic system testing framework that's easy to put to use on a daily basis is of great help.



# Goals

## Types of Projects

I need to set up a frame that would help me with automatic system testing of the following project types:

1. Web
	a. UI
	b. REST API
	c. WebSockets API
2. Mobile
	a. iOS
	b. Android
	(Ideally, cross-platform.)
3. Micro-services
	(via REST or WebSockets API)

## Types of Data

Most easily the frameworks can support plain text data. I believe I want to stick to plain text and call GNU diff tool to show me differences when comparing output (same as I did years ago for the automation framework developed for the "Tailored Software Development Process" project).

If the apps I want to test do move around binary data, I'd probably better write a JSON adapter inside the app and expose it via REST or WebSockets (for reasons similar to what I put in the Non-Goals section).

## Setting Up the Framework(s)

The framework(s) I find need to be easily set-up in a new, time-constrained project.

Setting up the framework for that particular project needs to be somewhere along the lines of:

- Adding contents from an existing package.json to the project's package.json and running `npm install`
- Adding to a python's requirements.txt from an existing requirements.txt and running `pip install`

## Framework Language

Ideally I want all my testing for the required project types to be in a single language, preferably Python.



# Non-Goals

## No Database Verification

I am specifically not proposing that the frameworks support database verification, i.e. you do an action in the tested app and then check in the database the results of that action.

One reason is there are many database engines, each with its quirks and API specifics. Another reason - even if you just consider a single DB engine, there is still a lot of work to connect to the DB and do the verification. The project "Messaging Router" has taught me that.

Instead, I am proposing that the DB is internal to the app I'm testing, and instead I verify the **results** that the app produces. Normally any change in the DB should be somehow reflected in the app's behavior.

## No Database Changing

Similarly to the previous item, I am not proposing that the frameworks support changing the database of an app to alter the latter's behavior for testing purposes.

The reasons are similar - mostly complexity of writing such testing code.

Instead, I maintain that the DB is internal to the app I'm testing, and if I need to alter the app's behavior I should have some API exposed for that. If that's not the case, then either that DB change is a hidden setting that the user will never see thus no need to test it, either I can expose a simple REST API from within the app just for testing purposes and this will be much simpler to implement.

## No Message Queues Sending or Reading

Similarly to the DB non goals, I do not want to verify message queues directly.

Micro-services do use message queues as a means of communications and I might need to use this some day. What I currently think I need is a way to verify that a micros-service processes a message I send to it and/or writes an output message I expect.

Based on my previous experience (such as for "Message Router" project), the simplest way to achieve this in both cases is to expose a REST (or maybe WebSockets) API from within the project that allows sending a message and retrieving of an output message of the micro-service.

The reason for this is that there are multiple, very different types of messaging queues (e.g. RabbitMQ, AWS SQS, Kafka - to name just a few), all with different APIs and not all of them supporting all languages. So on one hand I might not be able to access that particular message queue from my automated testing language of choice. On the other hand, setting up access to the messaging queue from the testing language can mean significant effort, as the "Message Router" has taught me. As opposed to this, the micro-service I need to test already uses the message queue so it should be much simpler to expose a REST API from it to allow automated testing.



# Analysis

## Appium

Supports: Mobile: iOS, Android, any kind of app.

http://appium.io/

https://github.com/appium-boneyard/sample-code/tree/master/sample-code/examples/python

TBD

## Selenium

Supports: Web: UI

TBD

## Katalon

Supports: Mobile with Appium, Web: UI, API

https://www.katalon.com/

https://www.altexsoft.com/blog/engineering/the-good-and-the-bad-of-katalon-studio-automation-testing-tool/

https://testguild.com/katalon-studio/

TBD

## (Something for WebSockets)

Couldn't find a tool that supports it out-of-the-box. Some suggest Katalon could support it via Java: https://forum.katalon.com/t/hi-can-we-make-automated-cases-for-web-socket-api-in-katalon-studio/25537

TBD

