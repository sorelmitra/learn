# Introduction

The Chat Mobile application is a demo chat app that talks to the Chat API.

# Overview

Currently the app doesn't do much. It just has the basic outgoing functionality of a "messenger" as shown in [Figure 1](#figure-1):

- Type a message that's being sent on ENTER
- Display the typed messages in a scrollable list
- Handle the animations and maintaining the scroll position when software keyboard is toggled
- Launch a POST request to the Chat API and show the status besides the message:
	- `(sending)` when the POST has been launched but the answer has not been received yet
	- `(sent)` when a success response was received
	- `(error!)` when either of these happened (details are logged to *JS console*):
		- Couldn't talk to the Chat API
		- An error response was received from the Chat API
		- A success message was received from the Chat API but not on the right message

#### Figure 1

![ChatMobile with POST error](https://github.com/sorelmitra/botagg/blob/master/docs/images/ChatMobile-POST-error.png "ChatMobile with POST error")
