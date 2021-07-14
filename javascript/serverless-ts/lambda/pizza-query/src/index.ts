import hl from 'hapi-lambda-sorel';
import api from './api';
import Routes from './routes/routes';
import Query from "./services/query";

let server;
export const handler = async (event, context) => {
	console.log("INIT");
	if (!server) server = await api.init(new Routes(new Query()));
	console.log("transformRequest: event", event);
	const req = hl.transformRequest(event, {});
	console.log("inject: request", req);
	const res = await server.inject(req);
	console.log("transformResponse: res", res);
	const response = hl.transformResponse(res);

	console.log("final response", res);
	return response;
};
