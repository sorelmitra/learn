## Code Search with exclusions

Search for code matching `GROUP` case sensitive, excluding code that matches `GROUP (ORDER` and `GROUP BY`, excluding various file extensions and paths, excluding repo:

    org:your-org /(?-i)\bGROUP\b/ NOT "group (order" NOT "group by" -path:__mocks__ -path:__tests__ -path:tf -path:yaml -path:sh -path:md -path:aws/dynamodb.js -path:util/dynamodb-v3.ts -repo:myorg/my-repo

## Render markdown files locally

https://github.com/joeyespo/grip

http://stackoverflow.com/questions/9843609/view-markdown-files-offline


## Workflow

### Run Workflow Based on Seed / SST

GitHub -> (your repo) -> Actions tab
-> 'Build, push, and trigger Seed deploy for SST Service'
-> Run Workflow -> 'Use workflow from' -> (your branch)
-> Run Workflow

Once the GitHub build completes, move to:

Seed UI -> (your org) -> (your service)
-> Overview tab -> Pipeline -> (your stage)

Wait for the build to end successfully.

### Run Workflows Across Repositories

The easiest way to do this would be to add a workflow to each repository that updates the corresponding area in Pages. I.e. in the "Main" repo, it would look something like:

    on: push

    jobs:
      main:
        steps:
          - name: Checkout
            uses: actions/checkout@v2

          - name: Build
            run: TODO

          - name: Publish
            run: TODO

And in the other repositories, you'd have something similar. For example in the Angular repository:

    on: push

    jobs:      
      angular:
        steps:
          - name: Checkout App
            uses: actions/checkout@v2

          - name: Build Angular
            run: |
              npm ci
              npm run build
              ng build --prod --base-href='angular'

          - name: Publish
            run: TODO

If you wanted to only publish when you update Main, you could have a workflow in your Main repository that builds and publishes all three, e.g.:

    on: push

    jobs:
      main:
        steps:
          - name: Checkout repo
            uses: actions/checkout@v2
            with:
              repository: my-org/main
              path: main

          - name: Build
            run: TODO
            working-directory: ./main

          - name: Publish
            run: TODO
            working-directory: ./main

      react:
        steps:
          - name: Checkout repo
            uses: actions/checkout@v2
            with:
              repository: my-org/react
              path: react

          - name: Build React
            run: TODO
            working-directory: ./react

          - name: Publish
            run: TODO
            working-directory: ./react

      angular:
        steps:
          - name: Checkout App
            uses: actions/checkout@v2
            with:
              repository: my-org/angular
              path: angular

          - name: Build Angular
            run: |
              npm ci
              npm run build
              ng build --prod --base-href='angular', 
            working-directory: ./angular

          - name: Publish
            run: TODO
            working-directory: ./angular


Source: https://stackoverflow.com/a/60289326/6239668
