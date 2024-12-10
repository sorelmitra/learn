# Overview

This is a simple Node.JS tool that allows processing arbitrary data in parallel, using Node.JS fork, in two steps:

- Step 1: Synchronously call a single Node.JS file that will gather the raw data to process.  Result: a CSV file with data to process in parallel.

- Step 2: Split the CSV file from Step 1 into N slices.  Feed each slice to a worker, which is
        another Node.JS file.  Spin up N workers and wait for them to finish.  Report results.

This is useful when you need to perform a lot of lengthy operations, and want to speed up the process by executing them in parallel.

NOTE: I also attempted to do this with just NodeJS promises, based on the fact that
requests are made in parallel by the Node engine.  It does run in parallel, but
much slower than in the fork version.  Thus, this tool was born.

# Quick Start

Copy `src/example/gatherer.js` and implement the `gather()` function to write the CSV file with the raw data.

Copy `src/example/worker.js` and implement the `doWork(number, slice, sliceSource, outputFilepath, logInterval)` function to:

- Read its `slice` from `sliceSource` CSV file, by calling to `getSlice(sliceSource, slice.start, slice.end)`
- Report progress by calling to `periodicReport('runtime', i, logInterval, count)`
- Process each element from the slice

Run the parallel tool like:

```shell
node src/index.js run -c 50 -g my-gatherer.js -w my-worker.js -f id,name,email -i 1000
```

## Chaining Workers

If you need multiple processing stages, you can create and chain as many different workers as you need.  The only requirements are that:

- The first worker in the chain has an initial gatherer that's fairly quick
- Each subsequent worker's gatherer can quickly transform `output/OUT.csv` from the previous stage, into `output/STEP1.csv` for the current stage

Then you'd run the chain like this:

Stage 1 (`gatherer1.js` is the initial gatherer):

```shell
node src/index.js run -c 50 -g gatherer1.js -w worker1.js -f id,name,email -i 1000
```

Stage 2 (`gatherer2.js` transforms Stage 1's `OUT.csv` into Stage 2's `STEP1.csv`):

```shell
node src/index.js run -c 50 -g gatherer2.js -w worker2.js -f id,name,email -i 1000
```

... and so on.

The intermediate `OUT.csv` files need not be lost: each subsequent gatherer can make a copy of it before transforming it into the next `STEP1.csv`.

# Reference

## CLI

Run like:

```shell
node index.js [--gatherer <gather-raw-data-JS>] --count <workersCount> --worker <worker-JS> [--worker-output-format <workerOutputFormat>]
```

Where:

* `workersCount` is the number of parallel workers that will be spawned.

* `workerOutputFormat` is the CSV format of the output file written by workers.  E.g. `id,name,email`.  Defaults to `id,value`.

* `gather-raw-data-JS` is the Node.JS file that will produce the CSV with raw data,
  which will be split into `workersCount` slices.  It will be called with a single parameter: `output-file-path.csv`, which is a CSV file where the program will write the data to be
        processed in parallel, with the first line containing the header row.  Example:
        
        id,name,email
        1,Doe,doe@foo.bar
        2,Pic,pic@foo.bar
  
  If gathering data is slow, you could spin up other workers instead, and skip this
  parameter.  Then run the script again to process the gathered data.

* `worker-JS` is the worker Node.JS file what will be called for each slice, and must
  produce a result file with its progress.  It will be called with the following parameters: `-n <worker-number> -s <slice-start> -e <slice-end> -i <report-interval> <slice-source>`, where:
  
  - `worker-number` is the unique number assigned to this worker
  - `slice-source` is the CSV raw data file gathered above, of which the slices are made
  - `slice-start` is the first non-header line number to process in that CSV file
  - `slice-end` is the last non-header line number to process in that CSV file
  - `report-interval` is the after how many items to report progress at the console.
    - Reporting progress is best done in a very few concise lines, and at least one.
    - Using in-line reporting such as dots with no new-line added is not recommended, because
    - the output would become useless due to the amount of workers sharing it.

## Functions

TBD
