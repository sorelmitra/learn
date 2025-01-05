export const memoize = (func) => {
  const funcResultCache = new Map();
  return (arg) => {
    const cachedValue = funcResultCache.get(arg);
    if (!cachedValue) {
      if (!arg) {
        funcResultCache.set(arg, func());
      } else {
        funcResultCache.set(arg, func(arg));
      }
    }
    return funcResultCache.get(arg);
  };
};

export const getStore = memoize(() => ({}));

