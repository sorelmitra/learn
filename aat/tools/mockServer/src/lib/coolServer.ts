import { ServerOptions } from 'https';
import * as Path from 'path';
import * as Hapi from '@hapi/hapi';
import * as Inert from '@hapi/inert';
import { aatServiceLog } from '../../../../lib/coolLog';

export const coolServer = (
  host: string,
  port: string,
  name?: string,
  tls?: boolean | ServerOptions
) => {
  const serviceName = name ? name : 'Basic Service';
  const LOG = aatServiceLog(serviceName);
  const routes = [
    {
      method: 'GET',
      path: '/hello',
      handler: async () => 'Hello, World!'
    },
    {
      method: 'GET',
      path: '/favicon.ico',
      handler: { file: 'favicon.ico' }
    }
  ];

  const plugins = [
    {
      plugin: Inert
    }
  ];

  const protocol = 'http';
  const server = new Hapi.Server({
    port: port,
    host: host,
    tls: tls,
    routes: {
      cors: true,
      payload: {
        allow: ['application/json']
      },
      files: {
        relativeTo: Path.join(__dirname, '')
      }
    }
  });

  server.events.on('response', request => {
    const responseCode = request.response['statusCode'] as number;
    LOG.debug(
      `${request.info.remoteAddress}: ${request.method.toUpperCase()} ${request.path} ${
        request.headers['content-type']
      } --> ${responseCode}`
    );
  });

  const start = async () => {
    await server.register(plugins);
    server.route(routes);
    await server.start();
    LOG.info('Started server', server.info);
    return Promise.resolve();
  };

  const stop = async () => {
    await server.stop();
    LOG.info('Stopped server', server.info);
    return Promise.resolve();
  };

  return { LOG, protocol, server, start, stop };
};
