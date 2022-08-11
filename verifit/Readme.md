# Overview

This is a Python library that simplifies setting up and using automatic system testing for several types of projects.

I've named it `verifit` as a contraction of `Verify It!`, i.e. "Make sure your system works fine!'



# Introduction

System testing (or application-level testing) is a must for every project. In a big company this is usually done by a dedicated QA team or perhaps by company-wide automatic testing policies. But what if you're an individual working on a project or, if for any reason, there's no QA in your project?

Automatic system testing comes to the rescue!

Usually you can't test a system 100% automatically. But, if you manage to cover the functionality (and perhaps some non-functional requirements, such as performance and high-availability - depending on the nature of the project), this will be a big help as the project builds up, and you have more and more features to test as you make progress developing it.

Also, on small projects or where you're time constrained, having an automatic system testing framework that's easy to put to use on a daily basis is of great help.



# Supported Apps and Data Types

This library can be used for apps that expose the following interfaces:

1. REST API
2. WebSockets API
3. GraphQL API
4. Anything that can expose one of the supported APIs above
5. Anything that has a Command Line Interface (CLI)
6. Web UI (outdated)
7. Mobile UI (outdated)

This library is designed for simplicity and ease of use.  Therefore, it doesn't support any of the following:

- _No database verification or changing of data_. Doing this would require a large amount of work. Moreover, DB should be internal to the app you're testing, and instead you verify the _results_ that the app produces. Normally any change in the DB should be somehow reflected in the app's behavior. Unless this is a weird app that interfaces with the world via a DB, in which case an _adapter should be written in the app's language_.
- _No message queues sending or reading_. This would also require a large amount of work, so better write an adapter in the app's language. The adapter can either expose a CLI or a REST API, that the test can use.
- _No binary data_. While it could technically be fitted in, it would pose some challenges especially with finding the differences between expected and actual data. If the app you're testing does move around binary data, better write an adapter in the app's language.



# Quick Start

## Setup

- Install Python 3.6 or higher
- Add library to `PYTHONPATH`: `export PYTHONPATH=<verifit repo>/src/lib:$PYTHONPATH`
- Install `dotenv`: `pip3 install python-dotenv`
- Install `appium`: `pip3 install Appium-Python-Client`

## Write a Hello World Test

Writing a test is as easy as:

1. Create a file `test.py`, like this:

        from runner import *
        def test_hello():
            expected, actual = runner.cli(["cp", "-v", get_input_filename(), get_output_filename()])
            assert actual == expected

2. Create a data file with the same name as the test function, `test_hello.json`:

        {
          "hello": "world"
        }

3. Run the test:

        ENV=dev pytest .

    It will complain that there's no `expected` file.

4. Since our app, `cp`, probably did its job correctly, just copy `test_hello-answer.json` to `test_hello-expected.json`.

5. Run the test again:

        ENV=dev pytest .

    This time it will pass.

Congrats! You wrote your first test!

## Write a REST Test

We will be testing a publicly available dummy REST server. It is basically as simple as the Hello World test above:

1. Create a file `.dev.env`. Add this line to it:

        REST_SERVER=https://jsonplaceholder.typicode.com

2. Create a file `test.py`, like this:

        from runner import *
        def test_placeholder():
            expected, actual = runner.rest(path='/posts', method='POST')
            assert actual == expected

3. Create a data file, `test_placeholder.json`, like this:

        {
            "title": "foo",
            "body": "bar",
            "userId": 1
        }

4. Run the test:

        ENV=dev pytest .

    It will complain that there's no `expected` file. 

5. Look at `test_placeholder-answer.json`. It contains the answer from our dummy server. If you're happy with the answer, you can go ahead and update the snapshot.

6. Update the snapshot:

        UPDATE_SNAPSHOT=1 ENV=dev pytest .

7. Run the test again:

        ENV=dev pytest .

    It will still pass.

## More Examples

You can explore more examples like these in the `src/tests` directory:

- `graphql`: A test for a GraphQL server. It uses a publicly available GraphQL server.
- `hello`: Similar to the Hello World test above.
- `localhost8201`: Web UI test. Unmaintained.
- `mobiledemo`: Mobile UI test. Unmaintained.
- `rest_api`: Similar to the REST test above. The example in this directory intentionally fails to show you how it looks when it does so.
- `websockets`: The demo API key is rotated every week, so you need to update that in `.dev.env` before running the test.

## Further Steps

The above examples are really what you'll be doing to create tests most of the time. 

Once you wrote the first test, just start writing more tests for your app. As you're doing this, you'll discover whether you're happy with the simple way, or you need to write more testing code (such as managing test data or organizing common test code into lib functions specific to your project).



# Reference

There's only one object you care about: `runner`.  It exposes several _functions_ you can use to run tests.  All of them functions do two things:

- Execute the test.
- Return a tuple of `expected, actual`, which represents the expected and actual results of the test.

Whenever the input or output files are used, their names are inferred from the test function name, of the form `<test_func><SUFFIX>`, where `SUFFIX` can be:

- `.json`: Input file (just the extension).  For GraphQL, this is the operation name
- `.graphql`: GraphQL query (where applicable)
- `.vars.json`: GraphQL variables (where applicable)
- `.vars.template.json`: GraphQL variables with placeholders of the form `${ID}` (where applicable)
- `-answer.json`: Output file
- `-expected.json`: Expected output file

The test functions:


- `cli`: Basic test runner that runs a command and returns the expected and actual output. Parameters:

    - `command`: The command to run.
    - `variables`: Placeholder replacements for your GraphQL variables. If you pass this in, then you need to create a `.vars.template.json` and in there you can use placeholders like `${ID}`. Then pass `variables={"ID": "1"}` to the `graphql` function.
    - `use_expected_output`: Whether to require the expected output file to be present.  If set to `False`, the value of `expected` from the returned tuple is `None`.
    - `strip_regex`: Strip the response body of content matching these regexes. This treats the response body as a string. The regexes are passed in as an array, e.g. `[r"a.*b", r"c.+d"]`.
    - `strip_keys`: Strip the response body of keys matching this regex. This treats the response body as a dictionary (JSON). The regexes are passed in as an array, e.g. `["data.posts", "data.indexes"]`.
    - `sort`: Sort the response body. This treats the response body as a list of dictionaries (JSON). The value of this parameter is an array of objects with the following fields:
        - `list`: The compound key to sort on. An empty list (`''`) name means sort the top list, otherwise it's a field designation, such as `'data.posts'`.
        - `field`: The field to sort on, e.g. `id`.
       
      Example: `[{"list": "data.posts", "field": "id"}]`
    - `sort`: 


- `login`: Wrapper over the `rest` function that uses the given `path`, `username`, `password` to log in to the REST server.


- `login_graphql`: Wrapper over the `graphql` function that uses the given `vars` to log in to the GraphQL server.


- `rest`: Wrapper over the `cli` function that builds a command that uses `cURL` to make a REST request to the REST server. Parameters:

    - `server`: If present, the REST server to use (without the path). If not present, the REST server is read from the `.dev.env` file, from the `REST_SERVER` variable.
    - `path`: The path to the REST endpoint.
    - `method`: The HTTP method to use.
    - `filetype`: Override the filetype to use for the input and output files. Uses `json` if not present.
    - `use_token`: Whether to use the API token when making the request.
    - `check_token`: Whether to check if a token is present in the response. (This is useful when you're testing the login endpoint.)
    - `use_input_file`: Whether to use the input file as payload for the request.
    - `input_data_raw`: Inline payload for the request.
    - `use_output_file`: Whether to save the response to the output file.  If set to `False`, the result of the request is ignored.
    - `retrieve_headers`: Whether to retrieve the headers from the response.
    - `follow_redirects`: Whether to follow redirects.
    - `variables`, `use_expected_output`, `strip_regex`, `strip_keys`, `sort`: Same as for the `cli` function.


- `graphql`: 

    - `server_public`: If present, the Public (unauthenticated) GraphQL server to use. If not present, it is read from the `.dev.env` file, from the `GRAPHQL_SERVER_PUBLIC` variable.
    - `server_private`: If present, the Private (authenticated) GraphQL server to use. If not present, it is read from the `.dev.env` file, from the `GRAPHQL_SERVER_PRIVATE` variable.
    - `use_token` , `check_token`: Same as for the `rest` function.
    - `variables`, `use_expected_output`, `strip_regex`, `strip_keys`, `sort`: Same as for the `cli` function.


- `websocket`: 

    - `server`: If present, the WebSocket server to use. If not present, it is read from the `.dev.env` file, from the `WEBSOCKETS_SERVER_URL` variable.
    - `ignore_messages`: An array of WebSocket response messages to ignore. (Useful with some servers that send periodic messages.)
    - `variables`, `use_expected_output`, `strip_regex`, `strip_keys`, `sort`: Same as for the `cli` function.



# UI Tests (outdated)

It is technically possible to write tests for Web UI and Mobile UI, too.

**Note**: UI tests weren't maintained, so some things might need adjustments. We are listing here the setup that worked last time we checked.

## Web UI Testing

Install Selenium: 
- `Selenium` library: `pip install selenium`.
- `WebDriver` binaries: download `chromedriver` from https://selenium.dev/documentation/en/webdriver/driver_requirements/#quick-reference and place them in PATH.

In your test file you essentially write Selenium code.

## Mobile UI Testing

1. Install Appium and Dependencies

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
    
2. Start the Android Simulator

    - Start ADB server for debugging: `adb start-server`.
    - List available simulator devices: `emulator -list-avds`.
    - Start simulator: `emulator @Nexus_5X_API_29_x86 &!`.

3. Start the iOS Simulator

    - Check the simulators you have: `xcrun simctl list`.
    - Boot your desired simulator: `xcrun simctl boot "iPhone 8"`.
    - Show your simulator on screen: Run the "Simulator" app on your Mac

4. Install the Test Apps in the Simulators

    - In the Android Simulator: `adb install /path/to/apk`
    - In the iOS simulator: `xcrun simctl install "iPhone 8" /path/to/ios/app`.
    - If you don't have Android or iOS apps to test and just want to play with this framework, you can find some test apps at https://github.com/appium/appium.git: `sample-code/apps/ApiDemos-debug.apk` and `sample-code/apps/TestApp.app`.

5. Start Appium

    - Run `appium` in your terminal.

In your `test_*()` methods, use the `driver_android` or `driver_ios` fixtures from `verifit`, depending on whether you're in a test for Android or iOS. Add test code in your `test_*()` methods, using Appium Python Client.

Quirks:

- Android doesn't work well with Java 11 and I did not bother to ruin my Java setup by trying to activate Java 8 temporarily. At least some tools don't work with Java 11: SDK Manager, UI Automator Viewer. The latter is the tool that's supposed to show you element IDs and other stuff used in automation.
- My Mac's Accessibility Inspector does not seem to work well with my "iPhone 8" simulator. So I couldn't see any label or ID on the Appium Test app.
- So I can't inspect mobile app's elements for automation, neither on Android nor iOS.
- The Python Client for Appium may not be that well documented.



# Side Notes

Some options investigated for automatic testing:

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

