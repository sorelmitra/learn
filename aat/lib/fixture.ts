import * as fs from 'fs';
import * as Path from 'path';

export const useFixtures = (innerPath = '.', basePath = 'fixtures') => {
  const path = Path.join(basePath, innerPath);
  const fixture = (name: string) => {
    const nameCandidates = [`${name}`, `${name}.txt`, `${name}.json`, `${name}.graphql`];
    for (const candidate of nameCandidates) {
      try {
        return fs.readFileSync(`${Path.join(path, candidate)}`).toString();
      } catch (err) {
        // empty
      }
    }
    throw Error(`Could not find any fixture <${name}> in ${path}`);
  };

  return { fixture };
};
