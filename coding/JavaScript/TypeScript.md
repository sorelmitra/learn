# Overview

TypeScript is a static **type checker** for JavaScript.  It detects existing JavaScript types and ensures proper usage of them.  It provides mechanisms for declaring JavaScript types (such as `string`) on variables and functions, and it allows declaring and using new types (such as `enum`).  It allows writing better tools such as IDEs.



# Introduction

## Basic Checks

- Proper usage of vars, operators, and functions (e.g. don't call a non-function)
- Null, undefined, non-existent identifiers
- Basic logic errors

## Type Declarations

- Variable: `const a: string = "a"`
- Function: `function getFavoriteNumber(x: string): number`: accepts a `string` parameter and returns a `number`

## Executable Code

Because TypeScript builds over JavaScript and is not implemented directly in browsers, its code needs to be converted to JavaScript in order to be usable.  That is our "executable" code.  This process of converting to the executable code is called `transpiling`.  During this process, TypeScript:

- Infers types from context, usually the more general type, i.e. `number` instead of literal type `0`
- Erases its specific types, declarations, and annotations when emitting
- Can target different JavaScript versions, e.g. converting template "back-tick" strings into plain string concatenation if needed

## Project Configuration

The presence of a `tsconfig.json` file in a directory indicates that the directory is the root of a TypeScript project. The `tsconfig.json` file specifies the root files and the compiler options required to compile the project.



# Transpilers and Bundlers

## Background

### Transpilers

**Transpilers**, or **source-to-source** compilers, are tools that read source code written in one programming language, and produce the equivalent code in another language. Languages you write that transpile to JavaScript are often called compile-to-JS languages, and are said to target JavaScript.

Reasons to use:

- Write in another language that you agree more on
- Use features from newer EcmaScript versions

Source: https://scotch.io/tutorials/javascript-transpilers-what-they-are-why-we-need-them

### Bundlers

These tools take a bunch of `.js` files that use modules (either CommonJS using require() statements, or ES Modules using import statements), and combine them into a single `.js` file. Some of them also allow specifying 'transformation steps', but their main purpose is bundling.

Why does bundling matter? While in NodeJS you have access to a module system that lets you load files as-needed from disk, this wouldn't be practical in a browser; fetching every file individually over the network would be very slow. That's why people use a bundler, which effectively does all this work upfront, and then produces a single 'combined' file with all the same guarantees of a module system, but that can be used in a browser.

Typical examples: Browserify, Webpack, Parcel.

Source: https://gist.github.com/joepie91/3381ce7f92dec7a1e622538980c0c43d

## TypeScript Transpilers & Bundlers

The TypeScript official transpiler is called `tsc`, install that with `npm install -g typescript`.  It checks `.ts` files, shows errors and warnings, and **emits**, i.e. compiles into plain JavaScript `.js` files.

Other options: Babel.  By using babel’s support for TypeScript, you get the ability to work with existing build pipelines and are more likely to have a faster JS emit time because Babel does not type check your code.

As for bundlers, any popular bundler will do, e.g. WebPack.



# Types

## Basic Types

- Primitives: `string`, `number`, `boolean`
- Array: `number[]` or `Array<number>`
- Tuple:
	- `[number]`: a 1-tuple with `number` at position `0`
	- `[string, number]`: a 2-tuple (pair) with `string` at pos `0` and `number` at pos `1`
- Object: `let pt: { x: number; y: number }`
	- Optional property: `obj: { first: string; last?: string }`
- Type alias: `type Point = { x: number; y: number; }` and `let pt: Point`
- Enum: `enum Direction { Up = 1, Down, Left, Right, }`

## Unions, Literals, Type Predicates & Narrowing

- Union: `id: number | string`
- Literal types:
	- `let x: "hello" = "hello";`, not very useful
	- `alignment: "left" | "right" | "center"`, quite useful with Union
- Narrowing: TypeScript understands a Union can be narrowed down to only some parts of it if you check for:
	- One of the types in it with `typeof`
	- Truthiness
	- Equality
	- Existence of a member with JavaScript's `in` operator
	- Appearance of another type's prototype in the the current variable's prototype chain with `instanceof`
	- Also if your code's control flow eliminate one of the types in that Union
- Type predicate: `pet is Fish`
	- Usage: `function isFish(pet: Fish | Bird): pet is Fish {return (pet as Fish).swim !== undefined;}`

## Interfaces, Type Intersections & Generics

- Interface: `interface Window { title: string }`
- Type intersection: 
	- `type Animal = { name: string }`
	- `type Bear = Animal & { honey: boolean }`
	- Now `Bear` contains all properties of `Animal` as well
- Generic type: `interface Box<Type> { contents: Type; }`

## Functions

- Function type expression: `fn: (a: string) => void`: a function with one parameter `a` of type `string`, that doesn’t have a return value
- Construct signature: `type SomeConstructor = {new (s: string): SomeObject;};`
	- Use it as `function fn(ctor: SomeConstructor) {return new ctor("hello");}`
- Generic function: `function firstElement<Type>(arr: Type[]): Type | undefined {return arr[0];}`
- Constrained generic: `function longest<Type extends { length: number }>(a: Type, b: Type) {...}`

## More Features

- Type assertion: `const myCanvas = document.getElementById("main_canvas") as HTMLCanvasElement;`
	- OR `const myCanvas = <HTMLCanvasElement>document.getElementById("main_canvas");`
- Non-null assertion operator: `let x: number | null; console.log(x!.toFixed()`
	- Compare with JavaScript's optional chaining `const dogName = adventurer.dog?.name;`
- Rest parameters: `function multiply(n: number, ...m: number[]) {return m.map((x) => n * x);}`
	- Compare with JavaScript's spread syntax: `function sum(x, y, z) {return x + y + z;}  const numbers = [1, 2, 3];  console.log(sum(...numbers));`
- Parameter de-structuring: `type ABC = { a: number; b: number; c: number };function sum({ a, b, c }: ABC) {console.log(a + b + c);}`

## Special Types

- `void`: no return value from a function
- `object`: non-primitive
- `unknown`: similar to `any` but safer since it cannot be used
- `never`: automatically assigned type when TypeScript detects the code cannot reach that point
- `Function`: callable object
- `null`, `undefined`: types for the corresponding JavaScript values
- `any`: accepts any type, to avoid unless forced by legacy or library code

## Creating Types from Types

TypeScript’s type system is very powerful because it allows expressing types in terms of other types:

- Generics - Types which take parameters
- Keyof Type Operator - Using the `keyof` operator to create new types
- Typeof Type Operator - Using the `typeof` operator to create new types
- Indexed Access Types - Using `Type['a']` syntax to access a subset of a type
- Conditional Types - Types which act like `if` statements in the type system
- Mapped Types - Creating types by `map`-ping each property in an existing type
- Template Literal Types - Mapped types which change properties via template literal strings

## Classes

TypeScript adds type annotations and other syntax to allow you to express relationships between classes and other types.



# Dynamic Imports

To make dynamic imports in TypeScript based on another module's path, you need to follow a few steps.  The documentation below is based on Jest with Node.JS, but can probably adapted to other engines as well.

## Required Configuration

First, you need to do the required config in your project:

In `package.json`:

      "devDependencies": {
        "@types/dotenv": "^8.2.0",
        "@types/jest": "^29.4.1",
        "@babel/preset-env": "^7.20.2",
        "@babel/preset-typescript": "^7.21.0",
        "jest": "^29.5.0",
        "ts-jest": "^29.0.5",
        "babel-jest": "^29.5.0",
        "typescript": "^4.9.5"
      },
      "dependencies": {
        "callsites": "^4.0.0",
      }

In `jest.config.js`:

    /** @typedef {import('ts-jest')} */
    /** @type {import('jest').Config.InitialOptions} */
    export default {
      roots: ['<rootDir>'],
      transform: {
        '^.+\\.tsx?$': [
          'ts-jest',
          {
            tsconfig: '<rootDir>/../tsconfig.json',
          },
        ],
        "^.+\\.(js|jsx)$": "babel-jest",
      },
      transformIgnorePatterns: ["node_modules/(?!(callsites)|(node-fetch)|(data-uri-to-buffer)|(fetch-blob)|(formdata-polyfill))"],
      moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
      setupFiles: ['<rootDir>/jest.setup.js']
    };

In `babel.config.json` - note that I had to use JSON and not JS config format!:

    {
      "presets": [
        [
          "@babel/preset-env",
          {
            "targets": {
              "node": "current"
            }
          }
        ],
        "@babel/preset-typescript"
      ]
    }

This whole config outlined above is done in order to avoid errors like:

- `Cannot use import statement outside a module`: https://stackoverflow.com/a/64223627/6239668
- `You appear to be using a native ECMAScript module configuration file, which is only supported when running Babel asynchronously`: https://stackoverflow.com/a/61948485/6239668
- `SyntaxError: Unexpected token export`: https://stackoverflow.com/a/49676319/6239668

More resources:
- https://nodejs.org/docs/latest/api/modules.html#__dirname
- Using `callsite` to get a caller's `dirname` - I ended up with the newer [callsites](https://www.npmjs.com/package/callsites) instead: https://stackoverflow.com/a/18145419/6239668
- https://mariusschulz.com/blog/dynamic-import-expressions-in-typescript

## Library Code

You can now write a small library that will import a TypeScript module dynamically, based on name, from the caller's directory.  It essentially boils down to:

`arg.ts:`

    export type NoneArg = undefined | null | void;
    export type ScalarArg = number | string;
    export type RecordArg = Record<string, any>;
    export type Arg = NoneArg | ScalarArg | RecordArg;
    export type FuncArg = ((arg: Arg) => Arg);

`driver.ts`:

    import {FuncArg} from "./arg";
    import callsites from "callsites";
    import * as path from "path";
    export const getDriver = (name) => {
        return async (functionalityName): Promise<FuncArg> => {
            const callSiteDir = path.dirname(callsites()[1].getFileName() ?? '.');
            const functionality = `${callSiteDir}/${name}_${functionalityName}`
            const module = await import(functionality);
            return module.execute;
        }
    };

## Client Code - Jest Test with a Driver

Assume you want to run a test via a driver.

For Jest, first add the global configuration for that driver in `jest.setup.js`:

    global.SHOPPING_DRIVER_NAMES = (process.env.SHOPPING_DRIVER ?? 'shopping-service,bla-bla-service').split(',');

You can call the library code from a test file that resides in another directory, like this - note the use of a Jest global variable to get the driver names array:

`login.test.ts`:

    import {getDriver} from "../../../src/lib/driver";
    describe.each<string>(global.SHOPPING_DRIVER_NAMES)(
      'products tests via %p',
      (driverName) => {
        test(`should fetch the products via ${driverName}`, async () => {
          await loginMainUserFromCache(`../login/${driverName}`);
          const data = await (await getDriver(driverName)('get-products'))(1);
          expect(data.id).toEqual(1);
          expect(data.title.length).toBeTruthy();
        });
      }
    );

## Run the Jest Test

You now run the test like this:

    SHOPPING_DRIVER='shopping-service,bla-bla-service' jest -c jest.config.js --verbose

You can also select to run only tests related to a particular driver by saying:

    SHOPPING_DRIVER='shopping-service' jest -c jest.config.js --verbose -t 'shopping-service'

This relies on the fact that `${driverName}` is being added to the test description, thus you can select tests by using it.




# Publish an NPM Package from a Project Folder

References:

- [The 30-second guide to publishing a TypeScript package to NPM](https://cameronnokes.com/blog/the-30-second-guide-to-publishing-a-typescript-package-to-npm/)
- [Discussion on publishing to NPM only a specific folder with JavaScript](https://stackoverflow.com/a/39946795/6239668)

## 1. Create the NPM Source Project

Create an NPM project, say the final `package.json` looks like below.  Here we assume that your library depends on `dotenv` and `jwt-decode`, and that you run your tests with Jest as an example:

    {
      "name": "acme-lib-code",
      "version": "1.0.0",
      "description": "Source code & tests for the acme library.",
      "repository": "https://github.com/acme/lib.git",
      "author": "Sorel Mitra <sorelmitra@yahoo.com>",
      "license": "MIT",
      "devDependencies": {
        "@babel/preset-env": "^7.20.2",
        "@babel/preset-typescript": "^7.21.0",
        "babel-jest": "^29.5.0",
        "jest": "^29.5.0",
        "ts-jest": "^29.0.5",
        "typescript": "*"
      },
      "dependencies": {
        "dotenv": "^16.0.3",
        "jwt-decode": "^3.1.2"
      }
    }

Create your folder that you want to publish, and add sources in it.  It will look like this (we're only showing relevant stuff):

    src
      |
      - lib
        |
        - a.ts
        - b.ts
    tests
      |
      - my.test.ts
    Readme.md
    package.json
    tsconfig.json
    yarn.lock

Create and test your code.

In your integration tests, you'll make use of the lib code by importing it like this:

    import { acme } from '../src/lib/a';
    import { acme2 } from '../src/lib/b';

## 2. Prepare Project for Publishing

Now assume you want to publish a library that allows you to import it like this using TypeScript:

    import { acme } from 'acme-lib/a';
    import { acme2 } from 'acme-lib/b';

For this, follow these steps:

**A)** Change your integration tests to use the above imports.

Of course they will fail, there's no `acme-lib` yet!

**B)** You will need a `dist` folder.  This is where you'll be publishing from.  This is a very important step, because you do not want your library to include the tests, nor do you want it to exist in `src/lib` folder once published.  It's also needed because you're publishing a TypeScript-ready package.

A future step will take care of this folder.  You don't need to do anything now, but keep all these in mind.

**C)** Prepare a package JSON that you will use when publishing.  This is yet another important step.  Because you're publishing only part of your source directory as a new root, you cannot use the same package JSON as your source code.  This is the reason for naming our source `package.json` like `acme-lib-code` - to differentiate it from the actual lib.

Create `src/publish/package.json`:

    {
      "name": "acme-lib",
      "version": "1.0.0",
      "description": "Acme library.",
      "main": "index.js",
      "types": "index.d.ts",
      "author": "Sorel Mitra <smitra@goodleap.com>",
      "license": "MIT",
      "dependencies": {
        "callsites": "^4.0.0",
        "dotenv": "^16.0.3",
        "jwt-decode": "^3.1.2"
      }
    }

Notice a few things:

- This package JSON is named `acme-lib` (as opposed to `acme-lib-code`), because this is our actual lib that we're publishing.
- We're putting `"main"` and `"types"` fields in here, they tell your library's client code where to look for the JavaScript, and respectively TypeScript declarations of your library.  So essentially here we're publishing a library that's usable for both JavaScript and TypeScript out-of-the-box.
- We're listing our library's dependencies.  Notice how we omitted the development dependencies that we have in the main package, because we're not publishing the tests.

**D)** Create the `index.ts` that will export all your library:

    export * from './a';
    export * from './b';

**E** Update your TypeScript configuration.

Change `tsconfig.json` to include these lines:

    {
      "compilerOptions": {
        "outDir": "dist",
        "declaration": true
      },
      "include": ["src/lib"]
    }

Here, we're making sure our output directory for TypeScript compilation is `dist`, and that the declarations are included.  We also make sure we only `include` folders that we want to publish.

**F)** Add a read-me for the package to be published.

The project's read-me probably has documentation for both development and usage of your library.  Now it's time you separate the two:

- Create `src/publish/README.md`, and put your library's end user documentation into it.
- Update `Readme.md` to only include development information and a link to the read-me for publishing.

**G)** Change `package.json` so it looks like below.  You're adding two things:

- Scripts for publishing.  They do the following:
	* Clean-up & recreate the `dist` folder.
	* Copy the `src/publish/` contents to the `dist` folder.
	* Compile your project into `dist` via `tsc`.
	* Run `yalc` in order to publish your package locally.  Very useful for testing before going into the NPM registry.
- A dependency of `acme-lib` into our development section.  We're preparing the land for fixing our tests, which now fail because they cannot find this lib yet.

    {
      "name": "acme-lib-code",
      "version": "1.0.0",
      "description": "Acme library.",
      "repository": "https://github.com/acme/lib.git",
      "author": "Sorel Mitra <sorelmitra@yahoo.com>",
      "license": "MIT",
      "scripts": {
        "prepublish": "rm -rfv dist && mkdir dist && cp -v src/publish/* dist/ && tsc",
        "local-publish": "yarn prepublish && cd dist && yalc publish --push"
      },
      "devDependencies": {
        "@babel/preset-env": "^7.20.2",
        "@babel/preset-typescript": "^7.21.0",
        "acme-lib": "^1.0.0",
        "babel-jest": "^29.5.0",
        "jest": "^29.5.0",
        "ts-jest": "^29.0.5",
        "typescript": "*"
      },
      "dependencies": {
        "dotenv": "^16.0.3",
        "jwt-decode": "^3.1.2"
      }
    }

**H)** Review your changes so far.

Your project structure now looks like this:

    src
      |
      - lib
        |
        - a.ts
        - b.ts
        - index.ts
      - publish
        | 
        - README.md
        - package.json
    tests
      |
      - my.test.ts
    package.json
    tsconfig.json
    yarn.lock

You added the `publish/package.json`, `publish/README.md` files, the `index.ts` source file, and the scripts for publishing in the main `package.json`.  You also configured `tsconfig.json` for publishing.

## 3. Do a Local Publish and Make It Work

**A)** Run `yarn local-publish` and check the results.  Go to `node_modules` and you should see a structure like this:

    node_modules
      |
      - acme-lib
        |
        - a.d.ts
        - a.js
        - a.js.map
        - b.d.ts
        - b.js
        - b.js.map
        - package.json
        - yalc.sig

If the structure doesn't look like this, e.g. you have different files or maybe some folders you don't expect, then review and redo the above steps.

**B)** Link the locally-published library.

In the project root (not in `dist`), run:

    yalc link acme-lib

**C)** Run the tests.

Check your test code that you changed earlier in order to import your lib.  The errors in it should now have disappeared.  (If not, review and redo the above steps or maybe restart your IDE.)

Run the tests, and they should pass as before.  If you encounter problems here, then maybe you forgot to export something, or check your `tsconfig.json` for wrong configuration.

Do any other checks you feel the need to, and then you can commit your changes and you're ready to go live!

## 4. Publish to NPM Registry

Change the version in these places:

- `package.json`, in fields:
	* `version`
	* `devDependencies` ➡️ `@loanpal/node-light-aat-lib`
- `src/publish/package.json`, in fields:
	* `version`


Login to NPM using your favorite user with `npm login`.  Now type:

    cd dist
    npm publish

## 5. Test the Published Library

We need to clean up our traces with `yalc`, in order to use the live library.

In the project root, run:

    yalc remove acme-lib

Now run `yarn install` and run your tests again.  If they pass, then you're done.  Else, start over and make sure you didn't forget or misdo a step.
