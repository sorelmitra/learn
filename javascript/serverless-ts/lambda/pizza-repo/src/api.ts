import * as Hapi from '@hapi/hapi';

import Routes from './routes/routes';
import plugins from './plugins';

const init = async (routes: Routes) => {
	// create base server
	const server = await new Hapi.Server({
		port: process.env.port || 3001,
		host: 'localhost',
		routes: {
			cors: true,
			payload: {
				allow: ['application/json'],
			},
		},
	});

	await server.register(plugins);
	server.route(routes.buildDefault());
	return server;
};

export default {
	init,
};
