# Introduction

The Chat API is a RESTful server that offers APIs for a basic chat service.

# API Reference

## Posts API

### Overview

Resource/Method | POST <br/>Create | GET <br/>Read | PUT <br/>Update | DELETE <br/>Delete
---|---|---|---|---
`/posts` | [Create Post](#create-post)  | <span style="color: Gray">*Not Implemented*</span>   | <span style="color: Gray">*Not Implemented*</span>   | <span style="color: Gray">*Not Implemented*</span>
`/posts/<id>` | <span style="color: LightCoral">Method not allowed (405)</span> | [Read Post with ID](#read-post-with-id) | <span style="color: Gray">*Not Implemented*</span>   | <span style="color: Gray">*Not Implemented*</span>

### Create Post

- **Request Type**: `POST`
- **Path**: `/posts`
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
    --data '{ \
        "body": "Hi there!" \
    }' \
    http://localhost:8000/posts
```

## Read Post with ID

- **Request Type**: `GET`
- **Path**: `/posts/<id>`
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
    http://localhost:8000/posts/1
```
