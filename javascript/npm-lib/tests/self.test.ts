import { doStuff } from 'npm-lib-sample';

describe('self-test', () => {
  it('should do stuff', async () => {
    const stuff = doStuff();
    expect(stuff.foo).toEqual('bar');
  });
});
