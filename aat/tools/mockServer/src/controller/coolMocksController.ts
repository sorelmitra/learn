import * as Nes from '@hapi/nes';
import { aatServiceLog } from '../../../../lib/coolLog';
import { aatServer } from '../lib/aatServer';
import {
  CoolMockOperation,
  CoolMockOpStatus,
  knownOperations,
  knownTargets
} from './coolMocksOperationRunner';

export const coolMocksController = async (host: string, port: string, name = 'Mocks Controller') => {
  const LOG = aatServiceLog(name);
  const { server, start: startService, stop: stopService } = aatServer(host, port, name);
  const start = () => startService();
  const stop = () => stopService();

  const execute = async (operation: CoolMockOperation): Promise<CoolMockOperation> => {
    const target = knownTargets.get(operation.target);
    const executor = knownOperations.get(operation.name);
    if (!executor) {
      const reason = { message: `Unknown operation <${operation.name}>!` };
      LOG.error(reason);
      return Promise.resolve({
        target: operation.target,
        name: operation.name,
        output: {
          status: CoolMockOpStatus.FAILURE,
          reason: reason
        }
      });
    }
    const result = await executor(target!, operation);
    LOG.info('Operation result:', JSON.stringify(result, null, 4));
    return Promise.resolve(result);
  };

  await server.register(Nes);
  const route = {
    method: 'POST',
    path: '/',
    config: {
      id: 'operation',
      handler: async (request: Record<string, any>) => {
        const operation = request.payload as CoolMockOperation;
        LOG.info('Received operation', operation);
        const output = await execute(operation);
        return output;
      }
    }
  };
  server.route(route);

  return { start, stop };
};
