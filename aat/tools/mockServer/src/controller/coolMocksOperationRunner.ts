import { client as WebSocketClient, connection } from 'websocket';
import Hapi from '@hapi/hapi';
import { aatDefaultLog } from '../../../../lib/coolLog';
import { mocksConfig } from '../config/aatToolsConfig';

export enum CoolMockOpName {
  SET_EXPECTED_INPUT = 'SET_EXPECTED_INPUT',
  SET_DESIRED_RESULT = 'SET_DESIRED_RESULT'
}

export type CoolMockOpInput = {
  payload?: Record<string, any>;
};

export enum CoolMockOpStatus {
  SUCCESS = 'SUCCESS',
  FAILURE = 'FAILURE'
}

export type CoolMockOpOutput = {
  status: CoolMockOpStatus;
  reason?: Record<string, any>;
  data?: Record<string, any>;
};

export type CoolMockOperation = {
  target: string;
  name: CoolMockOpName;
  input?: CoolMockOpInput;
  output?: CoolMockOpOutput;
};

export type CoolMockOpTarget = {
  setExpectedInput: (input: any) => void;
  setDesiredResult: (result: any) => void;
  name: string;
  routes: Hapi.ServerRoute[];
};

type CoolMockOpExecutor = (
  target: CoolMockOpTarget,
  operation: CoolMockOperation
) => Promise<CoolMockOperation>;

export type CoolWSPingMessage = {
  type: 'ping';
};

export type CoolWSExchangeMessage = {
  type: 'ping' | 'hello' | 'reauth' | 'request' | 'sub' | 'unsub' | 'message';
  id: string;
  version?: string;
  method?: 'GET' | 'POST';
  path?: string;
  payload?: CoolMockOperation | CoolWSPingMessage;
  message?: CoolMockOperation | CoolWSPingMessage;
};

export type CoolWSIncomingMessage = {
  type: 'utf8';
  utf8Data: string;
};

const LOG = aatDefaultLog();

const operationConfig = {
  host: mocksConfig.operations.host,
  port: mocksConfig.operations.port
};
const operationUrl = `ws://${operationConfig.host}:${operationConfig.port}`;

export const runMockOp = (target: string, name: CoolMockOpName, input: CoolMockOpInput = {}) =>
  new Promise<CoolMockOperation>((resolve, reject) => {
    const prefix = `[Operation ${target} ${name}]`;

    LOG.debug(`${prefix} START`);
    const ws = new WebSocketClient();

    const receiveOperationOutput = (receivedOp: CoolMockOperation) => {
      if (receivedOp.target !== target || receivedOp.name !== name) {
        LOG.warn(`${prefix} Received out-of-band output`, receivedOp);
        return;
      }
      const output = receivedOp.output;
      if (!output) {
        LOG.warn(`${prefix} Received empty output`, receivedOp);
        return;
      }
      if (output.status === CoolMockOpStatus.SUCCESS) {
        LOG.debug(`${prefix} DONE`, output);
        resolve(receivedOp);
      } else {
        LOG.debug(`${prefix} FAILED`, output);
        reject(receivedOp);
      }
    };

    const sendOperation = (conn: connection) => {
      const operation: CoolMockOperation = {
        target,
        name,
        input
      };
      const message: CoolWSExchangeMessage = {
        type: 'request',
        method: 'POST',
        path: '/',
        id: '2',
        payload: operation
      };
      conn.send(JSON.stringify(message));
      LOG.debug(`${prefix} Sent message`, message);
    };

    const sendPing = (conn: connection) => {
      const message: CoolWSExchangeMessage = { type: 'ping', id: '3' };
      conn.send(JSON.stringify(message));
      LOG.debug(`${prefix} Sent ping`, message);
    };

    const communicate = (conn: connection) => {
      LOG.debug(`${prefix} connected`);
      conn.on('message', data => {
        const message = data as CoolWSIncomingMessage;
        const exchangeMessage = JSON.parse(message.utf8Data) as CoolWSExchangeMessage;
        switch (exchangeMessage.type) {
          case 'hello':
            LOG.debug(`${prefix} received hello`);
            sendOperation(conn);
            break;
          case 'ping':
            LOG.debug(`${prefix} received ping`);
            sendPing(conn);
            break;
          case 'request':
            LOG.debug(`${prefix} received response`, exchangeMessage.payload);
            receiveOperationOutput(exchangeMessage.payload as CoolMockOperation);
            break;
          default:
            LOG.warn(
              `${prefix} received unhandled WS exchange message`,
              exchangeMessage.type,
              'content',
              data
            );
            break;
        }
      });
      conn.on('error', err => {
        LOG.error(`${prefix} error in connection:`, err);
        reject(err);
      });
      const hello: CoolWSExchangeMessage = { type: 'hello', id: '1', version: '2' };
      conn.send(JSON.stringify(hello));
    };

    ws.on('connect', connection => {
      communicate(connection);
    });
    ws.on('connectFailed', err => {
      LOG.error(`${prefix} could not connect:`, err);
      reject(err);
    });
    ws.connect(operationUrl);
  });

export const knownTargets = new Map<string, CoolMockOpTarget>();
export const knownOperations = new Map<CoolMockOpName, CoolMockOpExecutor>();

const runWithResult = async (operation: CoolMockOperation, callback: () => void) => {
  const result = {
    target: operation.target,
    name: operation.name,
    output: {
      status: CoolMockOpStatus.SUCCESS,
      reason: { message: `${operation.target} ${operation.name}` } as Record<string, any>
    }
  };
  try {
    callback();
  } catch (err) {
    result.output.status = CoolMockOpStatus.FAILURE;
    result.output.reason = { error: (err as Record<string, any>).toString() };
  }
  return Promise.resolve(result);
};

knownOperations.set(CoolMockOpName.SET_EXPECTED_INPUT, async (target, operation) => {
  return runWithResult(operation, () => target.setExpectedInput(operation.input));
});

knownOperations.set(CoolMockOpName.SET_DESIRED_RESULT, async (target, operation) => {
  return runWithResult(operation, () => target.setDesiredResult(operation.input));
});

export const addCoolMockOpTarget = (target: CoolMockOpTarget) => {
  knownTargets.set(target.name, target);
};
