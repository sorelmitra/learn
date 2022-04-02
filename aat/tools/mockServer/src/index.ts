import { aatServiceLog } from '../../../lib/coolLog';
import { mocksConfig } from './config/coolToolsConfig';
import { coolMocksController } from './controller/coolMocksController';
import { coolMockServer } from './server/coolMockServer';

const LOG = aatServiceLog('Mock Server');
LOG.start();

const main = async () => {
  const { start: startController } = await coolMocksController(
    mocksConfig.operations.host,
    mocksConfig.operations.port
  );
  const { start: startMockServer } = coolMockServer();
  await startController();
  await startMockServer();
};

main()
  .then(() => LOG.info('Mocks Controller started!'))
  .catch(err => LOG.error(err));
