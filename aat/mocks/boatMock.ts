import Hapi from '@hapi/hapi';
import { useFixtures } from '../lib/fixture';
import {
  CoolMockOpInput,
  CoolMockOpTarget
} from '../tools/mockServer/src/controller/coolMocksOperationRunner';
import { boatMockName } from './names';

const defaultboatMock = (): CoolMockOpTarget => {
  let expectedCondition: string;
  const { fixture } = useFixtures('boat');
  const name = boatMockName();

  const setExpectedInput = (input?: CoolMockOpInput) =>
    (expectedCondition = input?.payload?.condition as string);

  const setDesiredResult = () => {};

  const handle = (request: Hapi.Request) => {
    const error = 'Check request matches expectedCondition ...';
    if (error) {
      return error;
    }
    const response = JSON.parse(fixture('boatMockResponse')) as Record<string, any>;
    response.id = expectedCondition;
    return { body: JSON.stringify(response) };
  };

  const routes = [
    {
      method: 'POST',
      path: `/2015-03-31/functions/boat-service/invocations`,
      handler: handle
    }
  ];

  return { setExpectedInput, setDesiredResult, name, routes };
};

const boatMockInstance = defaultboatMock();

export const boatMock = () => {
  return boatMockInstance;
};
