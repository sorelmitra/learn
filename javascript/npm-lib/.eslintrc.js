module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  parserOptions: {
    project: ['./tsconfig.json'],
    tsconfigRootDir: __dirname
  },
  ignorePatterns: [],
  plugins: ['prettier', '@typescript-eslint', 'unused-imports'],
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:@typescript-eslint/recommended-requiring-type-checking',
    'prettier',
  ],
  rules: {
    // 'prettier/prettier': ['error', { endOfLine: 'auto' }],
    '@typescript-eslint/array-type': ['error', { default: 'array' }],
    'no-return-await': 'error',
    'unused-imports/no-unused-imports': 'error',
    '@typescript-eslint/explicit-member-accessibility': [
      'error',
      {
        accessibility: 'explicit',
        overrides: {
          accessors: 'off',
          constructors: 'no-public',
          methods: 'no-public',
          properties: 'off',
          parameterProperties: 'explicit'
        }
      }
    ],
    'no-console': 'off',
    '@typescript-eslint/no-empty-function': 'off',
    'class-methods-use-this': 'off',
    '@typescript-eslint/require-await': 'off',
    '@typescript-eslint/no-explicit-any': 'off',
    '@typescript-eslint/no-non-null-assertion': 'off',
    /*
    The ones below are because we're in testing code, and we
    use all sorts of unsafe calls & expressions by design.
     */
    '@typescript-eslint/restrict-template-expressions': 'off',
    '@typescript-eslint/no-unsafe-assignment': 'off',
    '@typescript-eslint/no-unsafe-call': 'off',
    '@typescript-eslint/no-unsafe-return': 'off',
    '@typescript-eslint/no-unsafe-member-access': 'off',
    '@typescript-eslint/no-unsafe-argument': 'off',
  },
  env: {
    jest: true,
    node: true,
  }
};
