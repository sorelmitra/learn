import { coolServer } from '../lib/coolServer';
import { mocksConfig } from '../config/coolToolsConfig';
import { useFixtures } from '../../../../lib/fixture';
import { addCoolMockOpTarget } from '../controller/coolMocksCommunicator';

import { mocks } from '../../../../mocks';

export const coolMockServer = () => {
  const { fixture } = useFixtures('.', 'aat/tools/mockServer/data/cert/server');
  const config = mocksConfig.server;
  const {
    server,
    start: startService,
    stop: stopService
  } = coolServer(config.host, config.port, 'Mock Server', {
    key: fixture('server.key'),
    cert: fixture('server.crt')
  });

  const findAndRegisterMocks = () => {
    mocks.forEach(mock => {
      const mockInstance = mock();
      server.route(mockInstance.routes);
      addCoolMockOpTarget(mockInstance);
    });
  };

  findAndRegisterMocks();

  // Mock input content type
  server.ext('onRequest', (request, h) => {
    request.headers['content-type'] = 'application/json';
    return h.continue;
  });

  const start = async () => startService();
  const stop = async () => stopService();

  return { start, stop };
};
