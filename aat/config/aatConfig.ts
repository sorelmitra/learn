export const aatConfig = () => {
  const getConfiguredLogChannel = () => {
    if (process.env.LOG_TO_CONSOLE) {
      return 'console';
    }
    return 'file';
  };

  const getConfiguredLogFilename = () => {
    const workerId = process.env.CUCUMBER_WORKER_ID
      ? `-worker-${process.env.CUCUMBER_WORKER_ID}`
      : '';
    return `aat${workerId}.log`;
  };

  return {
    getConfiguredLogChannel,
    getConfiguredLogFilename
  };
};
