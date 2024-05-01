
/* The data sets available to the tests */
export enum AatTestDataSet {
  DATA_SET_1 = 'DATA_SET_1',
  DATA_SET_2 = 'DATA_SET_2'
}

/* A test case to be executed against different organizations. */
export type AatDescribeWithDataSets = {
  /* The name of the test, it is passed to the test runner */
  name: string;
  /* The test implementation */
  callback: (orgType: AatTestDataSet) => any;
  /* Defines a list of data sets for which the test is to be skipped */
  skipFor?: AatTestDataSet[];
};
/* The list of all known organizations for running the tests on */
const getTestDataSets = (): AatTestDataSet[] => [AatTestDataSet.DATA_SET_1, AatTestDataSet.DATA_SET_2];
/*
Function to be used in order to define a test to be run against different data sets.
Same principles as for testWithDrivers.
 */
export const describeForEveryDataSet = ({ name, callback, skipFor }: AatDescribeWithDataSets) => {
  const dataSets = getTestDataSets().filter(d => !skipFor?.includes(d));
  return describe.each(dataSets)(`%p: ${name}`, (orgType: string) => callback(orgType as AatTestDataSet));
};
