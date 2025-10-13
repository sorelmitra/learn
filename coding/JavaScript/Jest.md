## Command line options

Ignore tests that match path:

	--testPathIgnorePatterns='.*transaction-search\.aat.test.ts.*'

Will ignore all tests from the specified file.

## Research - Skip Test Conditionally in Jest

### How You Can Skip with Jest

You can [skip one test](https://stackoverflow.com/a/48125240/6239668) in Jest with `test.skip()`.

You could even [skip it conditionally](https://stackoverflow.com/a/60438234/6239668) with:

    const skipIf = (condition) => condition ? test.skip : test;
    skipIf(Math.random() > 0.5)('test name', async () => {
      // Your test
    });

However, you [cannot specify a skip reason](https://github.com/facebook/jest/issues/11489).

Moreover, if you want that skipped test to appear into the report you have to use `todo` to duplicate the declaration of your skipped test like this:

    // Jest sucks: explicitly declare the skipped from below so that Jest reports it...
    test.todo(`MY-BUG-01 - skipped until bug is fixed - test name`);
    test.skip('test name', async () => {
      // Your test, Jest will siletly ignore it...
    });

It will then show up like this:

     PASS  tests/my.test.ts
      my tests
        ○ ...
        ✎ todo MY-BUG-01 - skipped until bug is fixed - test name

    Test Suites: 1 passed, 1 total
    Tests:       7 passed, 1 todo, 8 total

### You Can't Skip this Way with Jest

But you cannot skip a parameterized test that uses a table via `test.each()`:

The Jest team does not want to [allow to skip tests programmatically](https://github.com/facebook/jest/issues/7245).

One guy [proposed](https://github.com/facebook/jest/issues/7245#issuecomment-791285252) using `pending()` for this, but when I try it inside my test, I get `ReferenceError: pending is not defined`.

The [official Jest doc on `.each`](https://jestjs.io/docs/api#describeeachtablename-fn-timeout) doesn't say anything about skipping a test for a table row.  Not to mention that I couldn't find documentation for the form that I'm using with `test.each(array)`.  Closest thing [on this blog](https://dev.to/bgord/simplify-repetitive-jest-test-cases-with-test-each-310m).

The Jest team, as well as most of the community, is mostly ignoring another [feature request to programmatically skip test.each tests](https://github.com/facebook/jest/issues/9414).

### Conditional Skip Based on Row Values in Table-Based Testing in Jest

Finally, the answer came when I looked at this library: https://www.npmjs.com/package/jest-each.  It is actually incorporated into Jest, and probably the root of its `each` features.  That page shows that you can actually do `describe.each`, and thus you move the row data up one level, making it available before the test declaration.  So the solution is:

    describe.each<string>([1, 2, 3])(
      'test numbers %p',
      (currentNumber) => {
        test(`test the number ${currentNumber}`, async () => {
          expect(currentNumber).toBeTruthy();
        });

        const skipIf = (condition) => (condition ? test.skip : test);
        skipIf(currentNumber === 2)(`test again the number ${currentNumber}`, async () => {
          expect(currentNumber).toBeTruthy();
        });
      }
    );

Thus, the second test is skipped if number is 2.
