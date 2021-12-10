# Overview

This project aims to put together a schema that would simplify setting up and using automatic system testing frameworks for several types of projects.

I've named it `verifit` as a contraction of `Verify It!`, i.e. "Make sure your system works fine!'



# Introduction

System testing (or application-level testing) is a must for every project. In a big company this is usually done by a dedicated QA team or perhaps by company-wide automatic testing policies. But what if you're an individual working on a project or, if for any reason, there's no QA in your project?

Automatic system testing comes to the rescue!

Usually you can't test a system 100% automatically. But, if you manage to cover the functionality (and perhaps some non-functional requirements, such as performance and high-availability - depending on the nature of the project), this will be a big help as the project builds up, and you have more and more features to test as you make progress developing it.

Also, on small projects or where you're time constrained, having an automatic system testing framework that's easy to put to use on a daily basis is of great help.



# Goals

## Types of Projects

I need to set up a frame that would help me with automatic system testing of the following project types:

1. REST API
2. SOAP API
3. WebSockets API
4. GraphQL API
5. Micro-services (via one of the supported APIs above)
6. Web UI
7. Mobile UI, both iOS and Android, ideally cross-platform

## Types of Data

The frame will support only plain text data. See also non-goals below.

## Setting Up the Framework(s)

The framework will be easy to set up in a new, time-constrained project.

Setting up the framework for that particular project needs to be somewhere along the lines of:

- Adding to a python's requirements.txt from an existing requirements.txt and running `pip install`
- Creating a directory structure for the tests

## Framework Language

Ideally I want all my testing for the required project types to be in a single language, preferably Python.



# Non-Goals

## No Database Verification

I am specifically proposing that the framework does NOT support database verification, i.e. you do an action in the tested app and then check in the database the results of that action.

One reason is there are many database engines, each with its quirks and API specifics. Another reason - even if you just consider a single DB engine, there is still a lot of work to connect to the DB and do the verification. The project "Messaging Router" has taught me that.

Instead, I am proposing that the DB is internal to the app I'm testing, and instead I verify the **results** that the app produces. Normally any change in the DB should be somehow reflected in the app's behavior.

## No Database Changing

Similarly to the previous item, I am proposing that the framework does NOT support changing the database of an app to alter the latter's behavior for testing purposes.

The reasons are similar - mostly complexity of writing such testing code.

Instead, I maintain that the DB is internal to the app I'm testing, and if I need to alter the app's behavior I should have some API exposed for that. If that's not the case, then either that DB change is a hidden setting that the user will never see thus no need to test it, either I can expose a simple REST API from within the app just for testing purposes and this will be much simpler to implement.

## No Message Queues Sending or Reading

Similarly to the DB non goals, I do NOT want the framework to verify message queues directly.

Micro-services do use message queues as a means of communications, and I might need to use this some day. I think what I really need is a way to verify that a micro-service processes a message I send to it and/or writes an output message I expect.

Based on my previous experience (such as for "Message Router" project), the simplest way to achieve this in both cases is to expose a REST (or maybe WebSockets) API from within the project (either as a separate tool, either part of the actual app) that allows sending a message and retrieving of an output message of the micro-service.

The reason for this is that there are multiple, very different types of messaging queues (e.g. RabbitMQ, AWS SQS, Kafka - to name just a few), all with different APIs and not all of them supporting all languages. So on one hand I might not be able to access that particular message queue from my automated testing language of choice. On the other hand, setting up access to the messaging queue from the testing language can mean significant effort, as the "Message Router" has taught me. As opposed to this, the micro-service I need to test already uses the message queue, so it should be much simpler to expose a REST API from it to allow automated testing.

## No Binary Data

I want to stick to plain text and a tool to show me differences when comparing output (same as I did years ago for the automation framework developed for the "Tailored Software Development Process" project).

If the apps I want to test do move around binary data, I'd probably better write a JSON adapter inside the app and expose it via REST or WebSockets (for reasons similar to what I put in the Non-Goals section).



# The Framework

## Overall Status: PUBLISHED

## Overview

The framework uses Python to implement test cases and Pytest to drive discovery and execution of the tests.

According to [their page](https://docs.pytest.org/en/latest/getting-started.html), Pytest uses [standard test discovery practices](https://docs.pytest.org/en/latest/goodpractices.html#test-discovery).
For Pytest, test cases are Python file named `test_blah.py`, in an arbitrary directory tree.

For this framework, we have a few essential items:

1. A helper Python module, `verifit.py`, that executes a command with input (either parameters or file), reads its output (either from `stdin` or file), and offers back the expected and the received output.
2. A directory structure that allows for grouping test cases with their input and expected output data.
3. We run the tests by changing directory to the parent folder of the test cases and typing `pytest .`

The helper module resides in `src/verifit/verifit.py`.

The directory structure can be inspected in `src/test` and has the following key items:

- A subdirectory for each test suite or sub-suite, or whatever grouping makes sense for the project you're working on. E.g. `test/rest_api`.
- In this subdirectory the following items are **required**:
    - `test_blah.py`: Test file containing at least one test _function_.
		- `test_*()` _function_ inside the test file.  The test case itself.  It defines a name for the test, and the actual test code.
    - For some types of tests, that's all we need.  E.g. Web UI or Mobile don't usually use test input files.
- For tests that require input and output data, we **may** add other files:
    - `test_*.json` is the file to input for the commands in the test case. It will be created manually based on the test requirements.
    - `test_*-expected.json` is the expected output, or **snapshot**, of the test command.  It will be created manually based on the test requirements. The easiest way is to actually execute the test, verify the result, and once you're sure it's good, *update the snapshot*.
    - `test_*-answer.json` is the output got during running. Once you verified it's correct, you can overwrite `test_*-expected.json` with it.
    - Any other file a test case might need.
	- To *update the snapshot* do one of:
		- Manually copy `test_*-answer.json` over `test_*-expected.json`
		- Pass `updateSnapshot=True` to the `run_test()` function
		- Set environment variable `UPDATE_SNAPSHOT` to `1` or `True` before executing `pytest`.

## Binary Data Support

### Status: PUBLISHED.

Write your own tool to convert that binary data to JSON using libraries from the project you're testing. Call that tool as part of the test command of `test_*.py`.

## REST Support

### Status: PUBLISHED

### Installation

Install `curl`. How do you do that depends on your OS.

### Test Creation

Have your `test_*.py` do the following:

- Import `verifit`: `from verifit import *`.
- Define at least a `test_*()` function. In the function:
	- Define `command` to run `curl` with the needed parameters for the service you're testing.
	- Run the actual test: `expected, actual = run_test(command, name)`. 
	- Assert the result: `assert expected == actual`.

### Example

See example in `test/rest_api/test_rest_api_post_1.py`. The example sends to an online dummy API.

**Note**: The example intentionally fails to show you how it looks when it does so.

## SOAP Support

Do the same as for REST.

## WebSockets Support

### Status: PUBLISHED

### Installation

Install `vitwss`: add `src/verifit/` to your PATH and restart your terminal. ("vit" comes from "Verify It".)
It can do one of the following:
- Send message to web socket and exit.
- Wait for message on web socket for given timeout, then write the response and exit.

### Test Creation

Have your `test_*.py` do the following:

- Import `verifit`: `from verifit import *`.
- Define at least a `test_*()` function. In the function:
	- Define `trigger_command` to run the command that triggers WebSocket output, e.g. `vitwss` for sending data.
	- Define `background_test_command` to run the command that expects WebSocket output, e.g. `vitwss` for receiving data.
	- Run the actual test: `expected, actual = run_triggered_background_test(background_test_command, trigger_command, name)`. 
	- Assert the result: `assert expected == actual`.

### Example

See example in `test/websockets/test_websockets_1.py`. The example connects to an online WebSocket test server.

## GraphQL Support

### Status: PUBLISHED

### Installation

### Installation

Install `curl`. How do you do that depends on your OS.

Install `vitgql`: add `src/verifit/` to your PATH and restart your terminal. ("vit" comes from "Verify It".)
It takes a GraphQl template file and produces and launches a cURL request to the specified endpoint.

### Test Creation

Have your `test_*.py` do the following:

- Import `verifit`: `from verifit import *`.
- Define at least a `test_*()` function. In the function:
	- Define `command` to run `vitgql` with the needed parameters for the service you're testing.
	- Run the actual test: `expected, actual = run_test(command, name)`.
	- Assert the result: `assert expected == actual`.

### Example

See example in `test/graphql/test_graphql_1.py`. The example connects to an online GraphQL test server.

**Note**: The example intentionally updates snapshot to showcase that.

## Micro-Services Support

This is done via one of the supported APIs above, or by creating a tool similar to `vitwss` that adapts to whatever the micro-service uses for input/output.

## Web UI Support

### Status: PUBLISHED

### Installation

Install Selenium: 
- `Selenium` library: `pip install selenium`.
- `WebDriver` binaries: download `chromedriver` from https://selenium.dev/documentation/en/webdriver/driver_requirements/#quick-reference and place them in PATH.

### Test Creation

Have your `test_*.py` run Selenium WebDriver code. Tips:

- The `verifit` library is not used in this case. 
- Create a `login.py` with a common login function to call from your tests. Import it with `from login import *` as `pytest` adds your current test directory to `sys.path`.
- Extract duplicates in variables to make it easier to change them in case the UI changes.
- Group common functionality into a single test case to minimize the number of browser restarts and logins.
- As the UI allows it, have the test delete what it creates to avoid the need for a manual cleanup.
- Don't fall in the trap of implementing complex logic to do a cleanup that the UI does not support - if it doesn't, it's not important, and it's pretty easy to clean an entire table or so.

**Note**: You can't easily map UI testing on the "run command and check output" paradigm. After writing the sample I mention below, I believe Selenium is easy enough and worth the extra effort to test your Web UI app automatically. It gives you peace of mind when changing things.

### Example

See example in `test/localhost8201/test_localhost8201_content.py`.
- The example connects to `http://localhost:8201`. You need to have some sort of web app there.
- The example expects the web app to provide a login button, a login form with email and password; after login it expects a couple of links "Add Content" and "Your Content".
- In the "Add Content" page it expects a form that asks for Url, Title, Description and a submit button.
- In the "Your Content" page it expects links to the existing contents, each link having the name put in the Title field of the form. Clicking that links takes you to the "Edit Content" page.

**Note**: The example intentionally fails to show you how it looks when it does so.

## Mobile UI Support

### Status: PUBLISHED

There are some quirks, see at the end of this section.

### Installation and Setup

#### 1. Install Appium and Dependencies

- Get the latest Android Studio. Start the installer. Select "Custom". Check the tick box to have it create a simulator device for you.
- Add Android tools to path. Make sure you respect the order in PATH!

	```shell
	export ANDROID_HOME=/Users/sorel/Library/Android/sdk
	export PATH=$ANDROID_HOME/emulator:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:${PATH}
	```

- Install the latest XCode and open it to do post-installation steps.
- Install Carthage: `brew install carthage`
- Appium server: `npm install -g appium`.
- Appium Python Client: `pip install Appium-Python-Client`
- Pytest Sauce Client: `pip install SauceClient`

#### 2. Start the Android Simulator

- Start ADB server for debugging: `adb start-server`.
- List available simulator devices: `emulator -list-avds`.
- Start simulator: `emulator @Nexus_5X_API_29_x86 &!`.

#### 3. Start the iOS Simulator

- Check the simulators you have: `xcrun simctl list`.
- Boot your desired simulator: `xcrun simctl boot "iPhone 8"`.
- Show your simulator on screen: Run the "Simulator" app on your Mac

#### 4. Install the Test Apps in the Simulators

- In the Android Simulator: `adb install /path/to/apk`
- In the iOS simulator: `xcrun simctl install "iPhone 8" /path/to/ios/app`.
- If you don't have Android or iOS apps to test and just want to play with this framework, you can find some test apps at https://github.com/appium/appium.git: `sample-code/apps/ApiDemos-debug.apk` and `sample-code/apps/TestApp.app`.

#### 5. Start Appium

Run `appium` in your terminal.

### Test Creation

#### Create the Test Directory Configuration

Create a `conftest.py` file in each test directory, with a contents like this:

```python
import os
from verifit import configure_logging_and_screenshots
def pytest_configure(config):
	configure_logging_and_screenshots(config)
	config.ANDROID_APP = os.path.abspath('/path/to/apk')
	config.ANDROID_APP_ACTIVITY = '.your.app.activity'
	config.IOS_APP = os.path.abspath('/path/to/ios/app')
```

The file `conftest.py` is automatically discovered by Pytest. We import `verifit`, configure logging and screenshots, and define the app details for each platform. The fixtures in `verifit` pick those up when running the tests.

#### Create the Test Files

Have each of your `test_*.py` do the following:

- Import `verifit`: `from verifit import *`.
- Create a test class.
- Add `test_*()` methods to your class.
- In your `test_*()` methods, use the `driver_android` or `driver_ios` fixtures from `verifit`, depending on whether you're in a test for Android or iOS.
- Add test code in your `test_*()` methods, using Appium Python Client.

### Example

See examples in `test/websockets/`:

- For Android: `test_mobiledemo_android`. The example connects to Appium's sample Android `ApiDemos-debug.apk`.
- For iOS: `test_mobiledemo_ios`. The example connects to Appium's sample iOS `TestApp.app`.

**Note**: These examples don't fail intentionally, because I just copied and adapted the official Appium sample code.

### Quirks

- Android doesn't work well with Java 11 and I did not bother to ruin my Java setup by trying to activate Java 8 temporarily. At least some tools don't work with Java 11: SDK Manager, UI Automator Viewer. The latter is the tool that's supposed to show you element IDs and other stuff used in automation.
- My Mac's Accessibility Inspector does not seem to work well with my "iPhone 8" simulator. So I couldn't see any label or ID on the Appium Test app.
- So I can't inspect mobile app's elements for automation, neither on Android nor iOS.
- The Python Client for Appium may not be that well documented.

#### Solutions

- For Android, I need a Java 8 setup until further notice. I need to research how to have Java 8 and 11 working interchangeably on Mac.
- I need to do some more research on getting the element ID inspection work both on Android and iOS. The alternative is to know what IDs I put in the app I develop. 
- I need to study how does the official JavaScript Appium API maps to the Python Client.



# Other Investigated Options

- WebSockets: Couldn't find a tool that supports it out-of-the-box. Some suggest Katalon could support it via Java: https://forum.katalon.com/t/hi-can-we-make-automated-cases-for-web-socket-api-in-katalon-studio/25537.

- `Websocat` - https://github.com/vi/websocat.
	1. Pros: Supports both `ws` and `wss`.
	2. Cons: It can only read input and print to output. It can only send on Enter. How do I send JSON with it? How do I send multiline text? How do I receive only?
	3. Verdict: Not useful for my framework.

- `Katalon` - https://www.katalon.com/.
	1. Pros: Supports Mobile with Appium, Web UI, APIs. Nice GUI, they say.
	2. Cons: No Python support. You have to use their IDE, I can't integrate it easily with my framework. Seems to be aimed at QA departments. What I need for dev testing is much more simplistic, and I prefer scripts.
	3. Verdict: Not useful for my framework.
	4. Guide 1: https://www.altexsoft.com/blog/engineering/the-good-and-the-bad-of-katalon-studio-automation-testing-tool/. Guide 2: https://testguild.com/katalon-studio/.

