export const makeId = (proc: string) => (processorId: string) =>
  `${proc}_${processorId}`;
