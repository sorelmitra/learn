
class Query {
	constructor() {
	}

	run(request) {
		console.log("Request", request);
		let by = request.payload.by;
		return {
			myObject: {
				msg: `Hello, world by ${by}!`,
				statusCode: 0,
			}
		};
	}
}

export let queryService: Query = new Query();
