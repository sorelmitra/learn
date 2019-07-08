# Introduction

The Sample Bot is a web server that implements a simple chat bot. Currently it offers the following functionality:

Chat bot functionality, via WebSockets API:

- Registration of clients
- Receiving a message
- Sending back a response

# WebSockets API for Chat Bot v1

## Overview

All requests are sent on port `8202`.
The client is supposed to perform the action, then send the data specified.

## Register as a Chat Bot Client

### Step 1

- **Client Action**: `WebSocket Client Connect`
- **Path**: `/samplebot/v1`
- **Client Data**: N/A
- **Server Response**: Establishes connection

### Step 2

- **Client Action**: `WebSocket Message to Server`
- **Path**: N/A
- **Client Data**:
    ```json
    {
        "name": <name of client that registers>,
        "command": "register"
    }
    ```
- **Server Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "client-registration": {
                "id": <newly allocated registration id>,
                "name ": <name of client that registers>
            },
            "reason": "registration succeeded"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": false,
            "client-registration": {
                "id": null,
                "name ": <name of client that registers>
            },
            "reason": "failure reason"
        }
        ```

## De-register as a Chat Bot Client

- **Client Action**: `WebSocket Client Disconnect`
- **Path**: N/A
- **Client Data**: N/A
- **Server Response**: Disconnects and deregisters client

## Receive Chat Bot Response

- **Client Action**: N/A
- **Client Data**: N/A
- **Server Response**:
    When the chat bot responds, server will send the following data:
    ```json
    {
        "success": true,
        "botResponse": {
            "body": "message text"
        },
        "reason": "chat bot response"
    }
    ```

## Send Message to Chat Bot

- **Client Action**: `WebSocket Message to Server`
- **Path**: N/A
- **Client Data**:
    ```json
    {
		"name": <name of client>,
		"body": "message text"
    }
    ```
