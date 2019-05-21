# Introduction

The Chat Mobile application is a demo chat app that talks to the Chat API.

# Overview

Currently the app doesn't do much. It just has the basic outgoing functionality of a "messenger":

- Type a message that's being sent on ENTER
- Display the typed messages in a scrollable list
- Handle the animations and maintaining the scroll position when software keyboard is toggled
- Launch a POST request to the Chat API (That's where the error comes from as the Chat API is not yet there)

Figure 1:

![ChatMobile with POST error](https://github.com/sorelmitra/botagg/blob/master/docs/images/ChatMobile-POST-error.png "ChatMobile with POST error")
