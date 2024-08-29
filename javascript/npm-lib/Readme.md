# Library Documentation

If you're looking for the client library documentation, go to the [publishing readme](src/publish/README.md).

This document is about developing the library.

# Development

## Library

The library is located in `src/lib`, and the code uses Functional Programming principles.  Do your best to adhere to them when making changes.

Otherwise, the code is pretty straightforward.  Once you're done with your changes, do this to publish them:

**A)** Local Publish and Test

- Run `yarn local-publish`.  This will publish locally via `yalc`.
- Run `yalc link npm-lib-sample`.  This will link the locally-published package in the current project.
- Run the tests: `yarn test`.  They should pass.

**B)** Publish to NPM

Login to NPM using your favorite user with `npm login`.  Now type:

```shell
cd dist
npm publish
```

**C)** Test the Published Library

We need to clean up our traces with `yalc`, in order to use the live library.

In the project root, run:

```shell
yalc remove npm-lib-sample
```

Now run `yarn install` and run your tests again.  If they pass, then you're done.  Else, start over and make sure you didn't forget or misdo a step.

**References**:

For reference on publishing a certain folder from your project, and for publishing TypeScript projects, please consult these resources:

- [The 30-second guide to publishing a TypeScript package to NPM](https://cameronnokes.com/blog/the-30-second-guide-to-publishing-a-typescript-package-to-npm/)
- [Discussion on publishing to NPM only a specific folder with JavaScript](https://stackoverflow.com/a/39946795/6239668)


## Sample Tests

Change tests code, then make sure to test, document, & push your changes.
