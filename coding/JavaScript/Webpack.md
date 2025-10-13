# Overview

Webpack bundles all JavaScript contained in the `src` folder.  The `public` folder is copied as is.

React wraps Webpack and some other tools within `react-scripts`:

- `react-scripts start` bundles in-memory and serves from there
- `react-scripts build` bundles to a directory and serves from there

## Generated Code

- `main.chunk.js`: application code (React components)
- `vendors-main.chunk.js`: libraries code (dependencies)
- `bundle.js`: logic for loading the JS module

Files are split like that to leverage browser cache for unchanged files.
In production, the build number is added to force browser download the new version.
New chunks are added as the file size reaches a limit.

## Code Analysis

Webpack allows importing non-JS files, such as CSS.

