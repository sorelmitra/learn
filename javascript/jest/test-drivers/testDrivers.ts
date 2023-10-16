/*

Simple framework for testing with drivers and a
Domain Specific Language (DSL)

*/

const getEnv = getStoreReader(); // read from env
const setEnv = getStoreWriter(); // set in env

/* The driver names */
export enum TestDriver {
  DRIVER_ONE = 'driver-one',
  DRIVER_TWO = 'driver-two'
}

/* A driver selection for a particular command */
export type TestDriverSelection = {
  /*
  The list of available drivers for this command.
  It consists of an object that can have several keys,
  at most one for each of TestDriver values.
  For each key, a driver function is provided.
  The driver function accepts an argument as below.
  */
  drivers: Record<string, any>;

  /* The argument to the driver function */
  arg?: Arg;
};

/* The current driver name, taken from the environment */
const getDriverName = () => getEnv('DRIVER') as TestDriver;

/*
Selects and returns the driver function for a particular command
based on a driver selection.
It iterates through the provided drivers and choses the one whose
key matches the current driver name.
*/
export const testGetDriver = (driverSelection: TestDriverSelection) => {
  for (const key of Object.keys(driverSelection.drivers)) {
    if (key.toLowerCase() === getDriverName().toLowerCase()) {
      return driverSelection.drivers[key];
    }
  }
};

/* Calls the driver function returned by the getter above */
export const testCallDriver = (driverSelection: TestDriverSelection) => {
  return testGetDriver(driverSelection)(driverSelection.arg);
};

/* A test case to be executed with drivers. */
export type TestWithDrivers = {
  /* The name of the test, it is passed to the test runner */
  name: string;
  /* The test implementation */
  callback: () => any;
  /* Defines a list of driver names for which the test is to be skipped */
  skip?: TestDriver[];
};

/* The list of all known test drivers */
const getTestDrivers = (): TestDriver[] => ([TestDriver.DRIVER_ONE, TestDriver.DRIVER_TWO]);

/*
Function to be used in order to define a test to be ran with drivers.

Example usage:

const testDriverOneDoStuff = (config: TestDoStuffConfig) => console.log('Do stuff in DriverOne!');
const testDriverTwoDoStuff = (config: TestDoStuffConfig) => console.log('Do stuff in DriverTwo!');

const testDriverOneCheckDoStuff = (config: TestDoStuffCheckConfig) => {
  if (config.foo) {
    expect(config.response.driverOneFoo).toEqual(config.foo);
  }
}

const testDriverTwoCheckDoStuff = (config: TestDoStuffCheckConfig) => {
  if (config.foo) {
    expect(config.response.driverTwoFo).toEqual(config.foo);
  }
};

const testDoStuff = (arg: TestDoStuffConfig) => (testCallDriver({
  drivers: {
    [TestDriver.DRIVER_ONE]: testDriverOneDoStuff,
    [TestDriver.DRIVER_TWO]: testDriverTwoDoStuff
  },
  arg
}));

const testCheckDoStuff = (arg: TestDoStuffCheckConfig) => (testCallDriver({
  drivers: {
    [TestDriver.DRIVER_ONE]: testDriverOneCheckDoStuff,
    [TestDriver.DRIVER_TWO]: testDriverTwoCheckDoStuff
  },
  arg
}));

describe('stuff tests', () => {
  testWithDrivers({ name: "should do stuff", callback: async () => {
    const response = testDoStuff({ foo: 'bar' });

  }});
});
*/
export const testWithDrivers = ({ name, callback, skip }: TestWithDrivers) => {
  const testDrivers = getTestDrivers().filter(d => !(skip?.includes(d)));
  return test.each(testDrivers)(`%p: ${name}`, async (driver: string) => {
    setEnv('DRIVER')(driver);
    return await callback();
  })
};
