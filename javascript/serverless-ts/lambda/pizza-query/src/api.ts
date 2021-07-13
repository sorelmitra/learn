import * as Hapi from '@hapi/hapi';

import routes from './routes/base';
import plugins from './plugins';

const init = async () => {
  // create base server
  const server = await new Hapi.Server({
    port: process.env.port || 4017,
    host: 'localhost',
    routes: {
      cors: true,
      payload: {
        allow: ['application/json'],
      },
      response: {
        failAction: async (req, h, err) => {
          return err;
        },
      },
    },
  });

  await server.register(plugins);
  server.route(routes);
  return server;
};

export default {
  init,
};
