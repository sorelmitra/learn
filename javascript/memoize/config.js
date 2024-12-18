import dotenv from 'dotenv';
import { memoize } from './memoize';

const configure = () => {
  const environmentName = process.env['ENV'] ?? 'dev';
  const path = `.${environmentName}.env`;
  const values = dotenv.config({ path });
  return {
    ...values?.parsed,
    ...process.env,
    ENV: values?.parsed?.['ENV'] ?? environmentName
  };
};

const getStore = memoize(configure);

export const getStoreReader = () => {
  const store = getStore();
  return (key) => {
    return store[key];
  };
};

export const getStoreWriter = () => {
  const store = getStore();
  return (key) => {
    return (value) => {
      store[key] = value;
    };
  };
};
