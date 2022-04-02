import Hapi from '@hapi/hapi';
import { useFixtures } from '../lib/fixture';
import {
  CoolMockOpInput,
  CoolMockOpTarget
} from '../tools/mockServer/src/controller/coolMocksCommunicator';
import { pencilMockName } from './names';

const defaultPencilMock = (): CoolMockOpTarget => {
  let expectedColor: string;
  const { fixture } = useFixtures('pencil');
  const name = pencilMockName();

  const setExpectedInput = (input?: CoolMockOpInput) =>
    (expectedColor = input?.payload?.color as string);

  const setDesiredResult = () => {};

  const handle = (request: Hapi.Request) => {
    const error = `Check that ${request} matches expectedColor ...`;
    if (error) {
      return error;
    }
    const response = JSON.parse(fixture('pencilMockResponse')) as Record<string, any>;
    response.id = expectedColor;
    return { body: JSON.stringify(response) };
  };

  const routes = [
    {
      method: 'POST',
      path: `/2015-03-31/functions/pencil-service/invocations`,
      handler: handle
    }
  ];

  return { setExpectedInput, setDesiredResult, name, routes };
};

const pencilMockInstance = defaultPencilMock();

export const pencilMock = () => {
  return pencilMockInstance;
};
