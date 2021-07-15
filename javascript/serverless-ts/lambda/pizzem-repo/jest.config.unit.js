const rootConfig = require('./jest.config');
rootConfig.testRegex = `test.*\\.(test|spec)\\.(ts|js)x?$`;
console.log('RUNNING UNIT TESTS');
module.exports = rootConfig;
