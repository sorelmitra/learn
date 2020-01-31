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

1. REST API
2. WebSockets API
3. Micro-services (via REST or WebSockets API)
4. Web UI
5. Mobile UI, both iOS and Android, ideally cross-platform

## Types of Data

The frame will support only plain text data. See also non-goals below.

## Setting Up the Framework(s)

The framework(s) I find need to be easily set-up in a new, time-constrained project.

Setting up the framework for that particular project needs to be somewhere along the lines of:

- Adding contents from an existing package.json to the project's package.json and running `npm install`
- Adding to a python's requirements.txt from an existing requirements.txt and running `pip install`
- Creating a directory structure for the tests

## Framework Language

Ideally I want all my testing for the required project types to be in a single language, preferably Python.



# Non-Goals

## No Database Verification

I am specifically proposing that the frameworks do NOT support database verification, i.e. you do an action in the tested app and then check in the database the results of that action.

One reason is there are many database engines, each with its quirks and API specifics. Another reason - even if you just consider a single DB engine, there is still a lot of work to connect to the DB and do the verification. The project "Messaging Router" has taught me that.

Instead, I am proposing that the DB is internal to the app I'm testing, and instead I verify the **results** that the app produces. Normally any change in the DB should be somehow reflected in the app's behavior.

## No Database Changing

Similarly to the previous item, I am proposing that the frameworks do NOT support changing the database of an app to alter the latter's behavior for testing purposes.

The reasons are similar - mostly complexity of writing such testing code.

Instead, I maintain that the DB is internal to the app I'm testing, and if I need to alter the app's behavior I should have some API exposed for that. If that's not the case, then either that DB change is a hidden setting that the user will never see thus no need to test it, either I can expose a simple REST API from within the app just for testing purposes and this will be much simpler to implement.

## No Message Queues Sending or Reading

Similarly to the DB non goals, I do NOT want the frameworks to verify message queues directly.

Micro-services do use message queues as a means of communications and I might need to use this some day. I think what I really need is a way to verify that a micros-service processes a message I send to it and/or writes an output message I expect.

Based on my previous experience (such as for "Message Router" project), the simplest way to achieve this in both cases is to expose a REST (or maybe WebSockets) API from within the project (either as a separate tool, either part of the actual app) that allows sending a message and retrieving of an output message of the micro-service.

The reason for this is that there are multiple, very different types of messaging queues (e.g. RabbitMQ, AWS SQS, Kafka - to name just a few), all with different APIs and not all of them supporting all languages. So on one hand I might not be able to access that particular message queue from my automated testing language of choice. On the other hand, setting up access to the messaging queue from the testing language can mean significant effort, as the "Message Router" has taught me. As opposed to this, the micro-service I need to test already uses the message queue so it should be much simpler to expose a REST API from it to allow automated testing.

## No Binary Data

I want to stick to plain text and a tool to show me differences when comparing output (same as I did years ago for the automation framework developed for the "Tailored Software Development Process" project).

If the apps I want to test do move around binary data, I'd probably better write a JSON adapter inside the app and expose it via REST or WebSockets (for reasons similar to what I put in the Non-Goals section).



# The Framework

## Overview

The framework uses Python to implement test cases and Pytest to drive discovery and execution of the tests.

According to [their page](https://docs.pytest.org/en/latest/getting-started.html), Pytest uses [standard test discovery practices](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery).
For Pytest, test cases are Python file named `test_blah.py`, in an arbitrary directory tree.

For this framework, we have a few essential items:

1. A helper Python module, `verifit.py`, that executes a command with input (either parameters or file), reads its output (either from stdin or file), and offers back the expected and the received output.
2. A directory structure that allows for grouping test cases with their input and expected output data.
3. We run the tests by changing directory to the parent folder of the test cases and typing `pytest .`

The helper module resides in `src/verifit/verifit.py`.

The directory structure can be inspected in `src/test` and has the following key items:

- A subdirectory for each test suite or sub-suite, or whatever grouping makes sense for the project you're working on. E.g. `test/jsonplaceholder`.
- In this subdirectory the following items appear:
	- `test_*.py`: The test case itself. It basically defines a name for the test, a few commands to run, one of which being the actual test command. It then launches the commands and compares the output to the expected one.
	- `<name>-input.json` is the file to input for the commands in the test case. It will be created manually based on the test requirements.
	- `<name>-expected.json` is the expected output of the test command.  It will be created manually based on the test requirements. The easiest way is to actually execute the test, verify the result, and once you're sure it's good, copy it with this name.
	- `<name>-output.json` is the output got during running. Once you verified it's correct, you can overwrite `<name>-expected.json` with it.
		In the above, `<name>` must correspond to the variable `name` that's defined in `test_*.py`.
	- Any other file a test case might need. E.g. `websocketin-token.txt`, which contains the token for websocket.in that's used to demo the WebSockets test support.

## Binary Data Support

Write your own tool to convert that binary data to JSON using libraries from the project you're testing. Call that tool as part of the test command of `test_*.py`.

## REST Support

Install `curl`. How do you do that depends on your OS.

Have your `test_*.py` run `curl` as their single test command, using `verifit.run_test()`. 

See example in `test/jsonplaceholder/test_jsonplaceholder_post_1.py`. The example sends to https://jsonplaceholder.typicode.com/posts, which offers various dummy APIs.
Note: the example intentionally fails to show you how it looks when it does so.

## WebSockets Support

Install `vitwss`: add `src/verifit/` to your PATH and restart your terminal. ("vit" comes from "Verify It".)
It can do one of the following:
- Send message to web socket and exit.
- Wait for message on web socket for given timeout, then write the response and exit.

Have your `test_*.py` run `vitwss` as their background test command, using `verifit.run_triggered_background_test()`.

See example in `test/websocketin/test_websocketin_1.py`. The example connects to https://www.websocket.in/docs. It supports multiple users on a channel, sends to all of them. Supports authentication tokens.
Note: the example intentionally fails to show you how it looks when it does so.

## Micro-Services Support

This is done via REST or WebSockets support, by creating a tool similar to `vitwss` that adapts whatever the micro-service uses for input/output to either REST or WebSockets.

## Web UI Support

Install Selenium: 
- Selenium library: `pip install selenium`.
- WebDriver binaries: download from https://selenium.dev/documentation/en/webdriver/driver_requirements/#quick-reference and place them in PATH.

Have your `test_*.py` run Selenium WebDriver code. Tips:
- Create a `login.py` with a common login function to call from your tests. Import it with `from login import *` as `pytest` adds your current test directory to `sys.path`.
- Extract duplicates in variables to make it easier to change them in case the UI changes.
- Group common functionality into a single test case to minimize the number of browser restarts and logins.
- As the UI allows it, have the test delete what it creates to avoid the need for a manual cleanup.
- Don't fall in the trap of implementing complex logic do do a cleanup that the UI does not support - if it doesn't, it's not important and it's pretty easy to clean an entire table or so.
Note: You can't easily map UI testing on the "run command and check output" paradigm. After writing the sample I mention below, I believe Selenium is easy enough and worth the extra effort to test your Web UI app automatically. It gives you peace of mind when changing things.

See example in `test/localhost8201/test_localhost8201_content.py`. The example connects to `http://localhost:8201`. You need to have some sort of a web app. The example expects the web app to provide a login button, a login form with email and password; after login it expects a couple of links "Add Content" and "Your Content". In the "Add Content" page it expects a form that asks for Url, Title, Description and a submit button. In the "Your Content" page it expects links to the existing contents, each link having the name put in the Title field of the form. Clicking that links takes you to the "Edit Content" page.
Note: the example intentionally fails to show you how it looks when it does so.

## Mobile UI Support

Options:

- Appium - http://appium.io/
	1. Supports: Mobile iOS, Android; also other kind of apps, including desktop and Web UI.
	2. Sample code: https://github.com/appium/appium/tree/master/sample-code
	3. I've installed their TestApp into my iOS Simulator iPhone 8.

- Katalon - https://www.katalon.com/
	1. Supports: Mobile with Appium, Web UI, APIs
	2. Guide 1: https://www.altexsoft.com/blog/engineering/the-good-and-the-bad-of-katalon-studio-automation-testing-tool/
	3. Guide 2: https://testguild.com/katalon-studio/

TBD



# Other Investigated Options

- WebSockets:
	1. Couldn't find a tool that supports it out-of-the-box. Some suggest Katalon could support it via Java: https://forum.katalon.com/t/hi-can-we-make-automated-cases-for-web-socket-api-in-katalon-studio/25537
	2. Websocat - https://github.com/vi/websocat. The tool works nicely with `ws` or `wss`. But: It can only read input and print to output. It can only send on Enter. How do I send JSON with it? How do I send multiline text? How do I receive only?

