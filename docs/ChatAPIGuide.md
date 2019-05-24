# Introduction

The Chat API is a RESTful server that offers APIs for a basic chat service.

# API Reference

## Posts API v1

### Overview

Resource/Method | POST <br/>Create | GET <br/>Read | PUT <br/>Update | DELETE <br/>Delete
---|---|---|---|---
`/posts/v1/` | [Create Post](#create-post)  | [Read All Posts](#read-all-posts) | <span style="color: LightCoral">Method not allowed (405)</span> | [Delete All Posts](#delete-all-posts)
`/posts/v1/<id>/` | <span style="color: LightCoral">Method not allowed (405)</span> | [Read Post with ID](#read-post-with-id) | <span style="color: Gray">*Not Implemented*</span> | <span style="color: Gray">*Not Implemented*</span>

### Create Post

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
    http://localhost:8000/posts/v1/
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
    http://localhost:8000/posts/v1/
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
    http://localhost:8000/posts/v1/1/
```
