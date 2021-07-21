import hl from 'hapi-lambda-sorel';
import { DocumentClient } from 'aws-sdk/clients/dynamodb';
import {ApolloServer, Config, gql} from 'apollo-server-lambda';

import api from './api';
import DbDynamo from './db/dbDynamo';
import Routes from './routes/routes';
import Repo from "./services/repo";
import {typeDefs} from "./graphql/typeDefs";
import {Resolvers} from "./graphql/resolvers";
import {LambdaContextFunctionParams} from "apollo-server-lambda/dist/ApolloServer";

let server;

async function graphQlHandler(event, context) {
	console.log("Event", event);
	// return Promise.resolve("GraphQL is on the way");

	let resolvers: Resolvers = new Resolvers(new Repo(new DbDynamo(new DocumentClient())));
	let config: Config<LambdaContextFunctionParams> = {
		typeDefs: typeDefs,
		resolvers: resolvers.buildDefault(),
	};
	const server = new ApolloServer(config);

	let graphqlHandler = server.createHandler();
	return graphqlHandler(event, context, (error, result) => {});
}

export const handler = async (event, context) => {
	if (undefined !== event.body) {
		let o = JSON.parse(event.body);
		if (o.target == "graphql") {
			return graphQlHandler(event, context);
		}
	}

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
