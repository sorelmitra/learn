/** @typedef {import('ts-jest')} */
/** @type {import('jest').Config.InitialOptions} */
module.exports = {
  roots: ['<rootDir>'],
  transform: {
    '^.+\\.tsx?$': [
      'ts-jest',
      {
        tsconfig: '<rootDir>/../tsconfig.json'
      }
    ],
    '^.+\\.(js|jsx)$': 'babel-jest'
  },
  transformIgnorePatterns: [
    'node_modules/(?!(node-fetch)|(data-uri-to-buffer)|(fetch-blob)|(formdata-polyfill))'
  ],
  moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
  setupFiles: ['<rootDir>/jest.setup.js'],
};
