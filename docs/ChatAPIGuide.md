# Introduction

The Chat API is a RESTful server that offers APIs for a basic chat service.

# API Reference

## POST Message

- **Request Type**: `POST`
- **Path**: `/chat/posts/add`
- **Data**:

	```json
	{
		"body": "text to be posted"
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
    http://localhost:8000/chat/posts/add
```
