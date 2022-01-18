
class RootThing {
	myData: string;
}

export interface ITestThing {
  doStuff(): void;
  checkThing(): void;
}

export class TestThingFactory {
  static getInstance(): ITestThing{
    return new TestThing();
  }
}

class TestThing extends RootThing implements ITestThing {
  doStuff(): void {
    console.log('Doing stuff');
  }

  checkThing(): void {
    console.log('Checking thing');
  }
}
