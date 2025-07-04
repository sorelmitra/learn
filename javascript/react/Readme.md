# Quick Start

## Create the root for micro-frontends

Run and follow the prompts, choose the default options:

`npx create-single-spa --moduleType root-config`

In `tsconfig.json` -> `compilerOptions` add these:

```TypeScript
    "skipLibCheck": true,
    "module": "ES2022",
```

Downgrade `"webpack-config-single-spa-ts"` to `"^6.0.5"`.

Add `--port 9000` to the `start` command.

Now run `build` and `start`. Both should work.

## Create a micro-frontend (MFE) for inserting into the above root

Run and follow the prompts, choose the default options:

`npm init single-spa`

In `tsconfig.json` -> `compilerOptions` add these:

```TypeScript
    "module": "ES2022",
```

Downgrade `"webpack-config-single-spa-ts"` to `"^6.0.5"`.

Add `--port 8500` to both `start*` commands.

Now run `build` and `start`. Both should work.

## Register the new micro-frontend with the root

### Add shared dependencies

In `src/index.ejs` locate the following piece of code:

```HTML
  <!-- Shared dependencies go into this import map -->
  <script type="injector-importmap">
    {
      "imports": {
		// ...
	  }
	}
  </script>
```

And add the following inside `imports`:

```TypeScript
        "react":               "https://esm.sh/react@19.0.0?dev",
        "react-dom":           "https://esm.sh/react-dom@19.0.0?dev",
        "react-dom/client":    "https://esm.sh/react-dom@19.0.0/client?dev"
```

> Note: I also tried with this:

	```TypeScript
	        "react": "https://cdn.jsdelivr.net/npm/react@19.0.0/+esm",
	        "react-dom": "https://cdn.jsdelivr.net/npm/react-dom@19.0.0/+esm",
	```

> but it gave me `LOADING_SOURCE_CODE: Failed to resolve module specifier "react-dom/client"`.  I managed to fix it after iterating several times with ChatGPT o4-mini-high, which suggested changing my CDN for JS.  Since what I have for now is a `dev` version, I'm sure that for production another solution would be required.

### Add import for your MFE application

In `src/index.ejs` locate the following piece of code (note the `isLocal` thing):

```HTML
  <% if (isLocal) { %>
  <script type="injector-importmap">
    {
      "imports": {
		// ...
      }
    }
  </script>
  <% } %>
```

Locate your compiled app, e.g. `dist/AcmeInc-AncientLag.js`.
Check that this URL path is working in your browser, e.g. `http://localhost:8500/AcmeInc-AncientLag.js`.
Now add the following inside `imports`:

```TypeScript
        "@AcmeInc/AncientLag": "//localhost:8500/AcmeInc-AncientLag.js"
```

(!) Pay attention to the syntax with these imports!  It looks like JavaScript, but it doesn't allow a comma at the end of the object / array, so more probably it is a JSON.

### Register your MFE application

Look for `src/microfrontend-layout.html`. If it exists, then you're using single-spa Layout Engine.

#### If using single-spa Layout Engine

Remove the welcome app line:

```HTML
      <application name="@single-spa/welcome"></application>
```

Now add your app in place of the welcome, e.g. for the Layout Engine:

```HTML
      <application name="@organization/app-name"></application>
```

where `@organization/app-name` is the same name you used for the import step above,, e.g. `"@AcmeInc/AncientLag"`.

#### If NOT using single-spa Layout Engine

Remove the code for registering `@single-spa/welcome` as an application from `src/*root-config.ts` and add code for registering your `@organization/app-name` app.

# References

[1] https://single-spa.js.org/docs/getting-started-overview
[2] https://single-spa.js.org/docs/create-single-spa/
[3] https://react.dev/learn/creating-a-react-app
[4] https://github.com/single-spa/create-single-spa/issues/454#issuecomment-2770688845
[5] https://github.com/single-spa/create-single-spa/issues/380#issuecomment-2724747669
[6] https://react.dev/blog/2024/04/25/react-19-upgrade-guide
[7] https://jscomplete.com/learn/complete-intro-react
