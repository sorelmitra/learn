import hl from 'hapi-lambda-sorel';
import { DocumentClient } from 'aws-sdk/clients/dynamodb';

import api from './api';
import DbDynamo from './db/dbDynamo';
import Routes from './routes/routes';
import Repo from "./services/repo";

let server;
export const handler = async (event, context) => {
	console.log("INIT");
	if (!server) server = await api.init(new Routes(new Repo(new DbDynamo(new DocumentClient()))));
	console.log("transformRequest: event", event);
	const req = hl.transformRequest(event, {});
	console.log("inject: request", req);
	const res = await server.inject(req);
	console.log("transformResponse: res", res);
	const response = hl.transformResponse(res);

	console.log("final response", res);
	return response;
};
