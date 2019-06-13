# Introduction

The Chat API is a web server that offers APIs for a basic chat service:

- RESTful API for posting
- WebSockets API for getting posts notifications

# Posts RESTful API v1

## Overview

All requests are sent on port `8201`.

Resource/Method | POST <br/>Create | GET <br/>Read | PUT <br/>Update | DELETE <br/>Delete
---|---|---|---|---
`/posts/v1/` | [Create Post](#create-post)  | [Read All Posts](#read-all-posts) | <span style="color: LightCoral">Method not allowed (405)</span> | [Delete All Posts](#delete-all-posts)
`/posts/v1/<id>/` | <span style="color: LightCoral">Method not allowed (405)</span> | [Read Post with ID](#read-post-with-id) | <span style="color: Gray">*Not Implemented*</span> | <span style="color: Gray">*Not Implemented*</span>

## Create Post

- **Request Type**: `POST`
- **Path**: `/posts/v1/`
- **Data**:
	```json
	{
		"body": "text to be posted"
	}
	```
- **Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "post": {
                "id": <newly allocated post id>,
                "body": "text to be posted"
            },
            "reason": "message posted successfully"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": false,
            "post": {
                "id": null,
                "body": "text to be posted"
            },
            "reason": "failure reason"
        }
        ```

Example Request:

```bash
curl \
    --header "Content-Type: application/json" \
    --request POST \
    --data '{
        "body": "Hi there!" 
    }' \
    http://localhost:8201/posts/v1/
```

## Read All Posts

- **Request Type**: `GET`
- **Path**: `/posts/v1/`
- **Data**:
	```json
	{}
	```
- **Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "posts": [
                {
                    "id": <existing post id 1>,
                    "body": "message text"
                },
                {
                    "id": <existing post id 2>,
                    "body": "message text"
                },
                ...
            ]
            "reason": "messages retrieved"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": false,
            "post": null,
            "reason": "failure reason"
        }
        ```

Example Request:

```bash
curl \
    --header "Content-Type: application/json" \
    --request GET \
    --data '{}' \
    http://localhost:8201/posts/v1/
```

## Delete All Posts

- **Request Type**: `DELETE`
- **Path**: `/posts/v1/`
- **Data**:
	```json
	{}
	```
- **Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "posts": [],
            "reason": "all messages deleted"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": false,
            "posts": [],
            "reason": "failure reason"
        }
        ```

Example Request:

```bash
curl \
    --header "Content-Type: application/json" \
    --request DELETE \
    --data '{}' \
    http://localhost:8201/posts/v1/
```

## Read Post with ID

- **Request Type**: `GET`
- **Path**: `/posts/v1/<id>/`
- **Data**:
	```json
	{}
	```
- **Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "post": {
                "id": <existing post id (same as passed id)>,
                "body": "message text"
            },
            "reason": "message read successfully"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": false,
            "post": null,
            "reason": "failure reason"
        }
        ```

Example Request:

```bash
curl \
    --header "Content-Type: application/json" \
    --request GET \
    --data '{}' \
    http://localhost:8201/posts/v1/1/
```

# WebSockets API for Posts Notifications v1

## Overview

All requests are sent on port `8201`.
The client is supposed to perform the action, then send the data specified.

## Register for Posts Notification

### Step 1

- **Client Action**: `WebSocket Client Connect`
- **Path**: `/notifications/v1`
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
            "notification-registration": {
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
            "notification-registration": {
                "id": null,
                "name ": <name of client that registers>
            },
            "reason": "failure reason"
        }
        ```

## Read All Notification Registrations

- **Client Action**: `WebSocket Message to Server`
- **Path**: N/A
- **Client Data**:
	```json
	{
        "name": <name of client>,
        "command": "show-all"
	}
	```
- **Server Response**:
    - *Success*:
        ```json
        {
            "success": true,
            "notification-registrations": [
                ...,
                {
                    "id": <registration id N>,
                    "name ": <name of registered client N>
                },
                ...
            ],
            "reason": "registrations listed"
        }
        ```
    - *Failure*:
        ```json
        {
            "success": true,
            "notification-registrations": [
                {
                    "id": <registration id>,
                    "name ": <name of registered client>
                },
                ...
            ],
            "reason": "failure reason"
        }
        ```

## Delete Notification Registration

- **Client Action**: `WebSocket Client Disconnect`
- **Path**: N/A
- **Client Data**: N/A
- **Server Response**: Disconnects client

## Get Post Notification

- **Client Action**: N/A
- **Client Data**: N/A
- **Server Response**:
    When a message is posted, server will send the following notification:
    ```json
    {
        "success": true,
        "post": {
            "id": <post id>,
            "body": "message text"
        },
        "reason": "message posted notification"
    }
    ```
