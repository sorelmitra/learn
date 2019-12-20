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

## WebSockets

Couldn't find a tool that supports it out-of-the-box. Some suggest Katalon could support it via Java: https://forum.katalon.com/t/hi-can-we-make-automated-cases-for-web-socket-api-in-katalon-studio/25537

### Websocat

https://github.com/vi/websocat

The tool works nicely with `ws` or `wss`. It can only read input and print to output. It can only send on Enter.

How do I send JSON with it? How do I send multiline text? How do I receive only?

TBD

## REST

### Idea: Use cURL and Drive Tests via a Directory Structure

A directory with tests could look like this:

	- tests
		- tool.sh
		- jsonplaceholder
			- tool.sh
			- jsonplaceholder-post-1
				- tool-params.txt
				- jsonplaceholder-post-1-output-expected.json
				- jsonplaceholder-post-1-input.json
				- [created during test] jsonplaceholder-post-1-output.json
			- jsonplaceholder-post-2
				- jsonplaceholder-post-2-output-expected.json
				- jsonplaceholder-post-2-input.json
				- [created during test] jsonplaceholder-post-2-output.json
		- other-api
			- other-api-test-1
				- tool.sh
				- other-api-test-1-output-expected.json
				- other-api-test-1-input.json
				- [created during test] other-api-test-1-output.json
		- stuff
			- stuff-test-1
				- stuff-test-1-output-expected.json
				- stuff-test-1-input.json
				- [created during test] stuff-test-1-output.json

Where:

- `...-input.json` is the file to input to the tools
- `...-output-expected.json` is the expected output
- `...-output.json` is the output got during running.
- `tool.sh` is the program to run. If there's no such file in the current directory, look up in the hierarchy and stop at the first one found. For example in the above hierarchy, `stuff` uses `tests/tool.sh`; all `jsonplaceholder` tests use their own `tool.sh`, but `jsonplaceholder-post-1` adds some parameters to it, etc.
- `tool-params.txt` are extra params to the tool needed by that particular test. If there's no such file in the current directory just use `tool.sh`

The tool would be written so that it does the job to actually execute the test. For example, for a REST API, it could be a call to cURL. For a command line tool, it would be a call to the tool itself. For a Web UI, it would be a call to Python with a script such as EXAMPLE 1 of [this tutorial](https://www.guru99.com/selenium-python.html).

The problem: how do you solve the "running context" problem? I.e. if you need some context to be active at your test. For example, a Web UI test case might require some other UI actions, which means the browser must be kept open during the entire suite. What I'm proposing above means instantiating the browser for each test. If I am to keep it open, I must have some sort of a Python script waiting for commands.
Although, it could be that the only thing you need is login, which could be done by the `tool.sh` from the parent directory, i.e. the step only provides the actual actions, while the parent `tool.sh` starts the browser and logins, and executes the step actions afterwards.

### Idea: Use cURL and Drive Tests with Pytest and a Directory Structure

The idea with directory structure is good, but requires me to write and maintain my own test runner. This means both a lot of work and would not be easy to explain why I chose this path.

According to [their page](https://docs.pytest.org/en/latest/getting-started.html), Pytest uses [standard test discovery practices](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery). These practices seem to suit my needs.

The idea would be that I use the same directory structure, but instead of `tool.sh` I'd have `test_blah.py`. This is my entry point and it really helps to have it in Python rather than Bash, as it's rather easy to write cross-platform code in Python.

For this, I would only write a Python library that executes a command with input (either parameters or file), reads its output (either from stdin or file), and compares it to an expected output.

See an implementation on this in `test_jsonplaceholder_post_1.py` and `verifit.py` - the latter must be in a directory that's included in `PYTHONPATH`.

TBD

## Appium

Supports: Mobile: iOS, Android, any kind of app.

http://appium.io/

https://github.com/appium/appium/tree/master/sample-code

I've installed their TestApp into iOS Simulator iPhone 8.

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

