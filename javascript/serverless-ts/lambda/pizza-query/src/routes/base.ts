
export default [
	{
		method: 'GET',
		path: '/pizzemQueryPizza',
		handler: (request, h) => {
			return h.response({
				myObject: {
					msg: 'Hello, world!',
					statusCode: 0,
				},
				serverless: request.serverless
			})
		}
	}
];
