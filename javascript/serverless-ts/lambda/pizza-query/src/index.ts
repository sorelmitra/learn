import hl from 'hapi-lambda-sorel';
import api from './api';

let server;
export const handler = async (event, context) => {
	console.log("INIT");
	if (!server) server = await api.init();
	console.log("transformRequest: event", event);
	const req = hl.transformRequest(event, {});
	console.log("inject: request", req);
	const res = await server.inject(req);
	console.log("transformResponse: response", res);
	const response = hl.transformResponse(res);

	return response;
};
