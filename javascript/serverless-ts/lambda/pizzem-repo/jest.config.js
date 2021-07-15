/** @typedef {import('ts-jest')} */
/** @type {import('jest').Config.InitialOptions} */
module.exports = {
  roots: ['<rootDir>'],
  transform: {
    '^.+\\.(ts|tsx|js)?$': 'ts-jest',
  },
  moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
  globals: {
    'ts-jest': {
      tsconfig: '<rootDir>/tsconfig.json',
    },
  },
};
