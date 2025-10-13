### How to query a Global Secondary Index

You need to run a query on the secondary index, not an item read from the table.

	var email = "JoeBloggs@hotmail.com";

	var docClient = new AWS.DynamoDB.DocumentClient();

	var params = {
	    TableName : "Users",
	    IndexName : "EmailIndex",
	    KeyConditionExpression: "#email = :v_email",
	    ExpressionAttributeNames:{
	        "#email": "Em"
	    },
	    ExpressionAttributeValues: {
	        ":v_email": email
	    }
	};

	docClient.query(params, function(err, data) {
	    if (err) {
	        console.error("Unable to query. Error:", JSON.stringify(err, null, 2));
	    } else {
	        console.log("Query succeeded.");
	        data.Items.forEach(function(item) {
	            console.log(" -", item.Em + ": " + item.Ai);
	        });
	    }

