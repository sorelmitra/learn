/* eslint-disable */
/* eslint-disable prettier/prettier */
console.time('runtime')

process.env = {
  ...process.env,
  AWS_SDK_LOAD_CONFIG: true,
  AWS_PROFILE: `${process.env.ENV}-profile`,
  AWS_NODEJS_CONNECTION_REUSE_ENABLED: '1'
}
const fs = require('fs')
const { format } = require('@fast-csv/format')
const { parse } = require('@fast-csv/parse')
const { ParallelScanIterator } = require('@aws/dynamodb-query-iterator')
const DynamoDB = require('aws-sdk/clients/dynamodb')
const { DateTime } = require('luxon')
const { fork } = require('child_process');
const { DocumentClient } = DynamoDB
const ddb = new DocumentClient()

const { unmarshall } = DynamoDB.Converter


const isMatch = tripNames => {
  return ['Skiatos', 'Tenerife', 'Florida'].some(item => tripNames.includes(item))
}

const getBoatTrips = async boatId => {
  const { Items: trips } = await ddb.query({
    TableName: `${process.env.ENV}-boat-trips`,
    KeyConditionExpression: 'boatId = :boatId',
    ExpressionAttributeValues: { ':boatId': boatId },
    ExpressionAttributeNames: { '#timestamp': 'timestamp' },
    ProjectionExpression: 'tripName, #timestamp'
  }).promise()
  return trips.map(trip => trip.tripName)
}

const isSloop = boatType => boatType === 'sloop';

const dateDiffInDays = (date1, date2) =>
  Math.round(DateTime.fromISO(date2).diff(DateTime.fromISO(date1), 'days').days);

const isRecent = launchedOnMarketDate => {
  const diffInDays = dateDiffInDays(DateTime.local().toString(), launchedOnMarketDate);
  return diffInDays > -150;
};

let foundBoatsCount = 0

async function checkTrips(number, boatId, stream) {
  const tripNames = await getBoatTrips(boatId);
  // console.log(`TripNames [${id}]: `, tripNames)

  if (isMatch(tripNames)) {
    // console.log(`Worker ${number}: found ${boatId}`, tripNames.join(","))
    foundItemsCount++
    stream.write([boatId, tripNames.join(",")])
  }
}

const step1Filename = "./dynamodb-parallel-scanner-STEP1.csv";

const getStep1Items = async () => {
  const iterator = new ParallelScanIterator(new DynamoDB(), {
    TableName: `${process.env.ENV}-boats`,
    ProjectionExpression: "id, boatType, constructor",
    TotalSegments: 8,
    ReturnConsumedCapacity: "INDEXES"
  });

  const stream = format({ headers: ["id"] });
  stream.pipe(fs.createWriteStream(step1Filename));

  let i = 0;
  for await (const record of iterator) {
    if (++i % 10000 === 0) {
      console.log(`still running... ${i}`);
      console.timeLog("runtime");
    }
    // console.log(`###: `, record)
    const unmarshalled = unmarshall(record);
    // console.log(`###: `, unmarshalled)
    const { id, boatType, constructor } = unmarshalled;
    if (isSloop(boatType) && !isRecent(constructor.launchedOnMarketDate)) {
      // console.log('Match: ', { id, boatType, constructor.launchedOnMarketDate })
      stream.write([id]);
    }
  }

  console.log("iterator.count: ", iterator.count);
  console.log("iterator.scannedCount: ", iterator.scannedCount);
  console.log("iterator.consumedCapacity: ", iterator.consumedCapacity);

  stream.end();
};

async function getStep1ItemsCount() {
  return new Promise(resolve => {
    fs.createReadStream(step1Filename)
      .pipe(parse())
      .on("error", error => console.error(error))
      .on('data', () => {})
      .on("end", (rowCount) => {
        let itemCount = rowCount - 1 // skip header row
        console.log(`${itemCount} items from step 1`)
        resolve(itemCount)
      })
  })
}

function LinkedList() {
  this.value = undefined
  this.next = null
  return this
}

const getStep1ItemSlice = (start, end) => new Promise(resolve => {
  let i = -1
  const itemSlice = new LinkedList(null, null)
  let current = itemSlice
  fs.createReadStream(step1Filename)
    .pipe(parse())
    .on("error", error => console.error(error))
    .on("data", async row => {
      if (row == 'id') return // skip header row
      i++
      if (i % 100 === 0) await new Promise(resolve => setTimeout(resolve, 10)); // sleep a bit
      if (i < start || i >= end) return;
      current.value = row[0]
      current.next = new LinkedList()
      current = current.next
    })
    .on("end", (rowCount) => {
      // let s = ''
      // for (current = itemSlice; current.next != null; current = current.next) {
      //   s = `${s} ${current.value}`
      // }
      // console.log(s)
      resolve(itemSlice)
    })
})

const getSlices = (itemsCount, workersCount) => {
  const size = Math.floor(itemsCount / workersCount)
  const slices = []
  let start = 0
  let end = 0
  for (let i = 0; i < workersCount - 1; i++) {
    end = start + size
    slices.push({start, end})
    start = end
  }
  slices.push({start, end: itemsCount + 1})
  console.log('Slices for workers', slices)
  return slices
}

const checkStep1ItemSlice = async (number, { start, end }) => {
  const n = end - start
  const header = `Worker ${number}, item slice ${start} - ${end}`
  console.log(`${header}: START`)
  const itemSlice = await getStep1ItemSlice(start, end)
  const stream = format({ headers: ['id', 'trips'] });
const step2Filename = `./logs/dynamodb-parallel-scanner-WORKER-${number}-OUT.csv`;
  stream.pipe(fs.createWriteStream(step2Filename));
  let i = 0
  for (let current = itemSlice; current.next != null; current = current.next) {
    const id = current.value
    await checkTrips(number, id, stream)
    if (++i % 100 === 0) {
      console.timeLog('runtime', `${header}: at item ${i} / ${n}; ${foundItemsCount} matched so far`)
    }
  }
  stream.end()
}

const controllers = []

const exitHandler = (options, exitCode = 0) => {
  for (const c of controllers) {
    if (c) c.abort()
  }
  console.timeEnd("runtime");
  if (options.exit) process.exit(exitCode)
}

process.on('exit', exitHandler.bind(null,{exit: true}));
process.on('SIGINT', exitHandler.bind(null, {exit: true}));

const forkToCheckStep1ItemSlice = async (index, slice) => new Promise(resolve => {
  const number = index + 1
  controllers[index] = new AbortController()
  const { signal } = controllers[index]
  const child = fork(__filename, [number, slice.start, slice.end], {signal})
  child.on('close', (code) => {
    console.log(`child process exited with code ${code}`);
    resolve()
  })
})

const getWorkerResults = async (number, itemSlice) => new Promise(resolve => {
  let i = -1
  let current = itemSlice
  fs.createReadStream(getStep2Filename(number))
    .pipe(parse())
    .on("error", error => console.error(error))
    .on("data", async row => {
      if (row.includes('id')) return // skip header row
      i++
      current.value = row
      current.next = new LinkedList()
      current = current.next
    })
    .on("end", (rowCount) => {
      resolve(itemSlice)
    })
});

const gatherAllResults = async slices => {
  const stream = format({ headers: ['id', 'trips'] });
  stream.pipe(fs.createWriteStream(step2AllFilename));
  const itemSlice = new LinkedList(null, null);
  for (let i = 0; i < slices.length; i++) {
    const results = await getWorkerResults(i + 1, itemSlice);
    for (let current = results; current.next != null; current = current.next) {
      stream.write(current.value)
    }
  }
  stream.end();
};

const getStep2Items = async (workersCount) => {
  const n = await getStep1ItemsCount()

  const slices = getSlices(n, workersCount)
  const children = []
  for (let i = 0; i < slices.length; i++) {
    children[i] = forkToCheckStep1ItemSlice(i, slices[i])
  }
  await Promise.all(children)
  await gatherAllResults(slices);
  console.log(`All ${children.length} children have finished`)
}

/**
 * Find items from a primary DynamoDB table where certain conditions are met.  Then on the found items run a "join" query on a secondary table where the ID from the primary table is a foreign key.  Run this in parallel forked processes to greatly speed up the terrible slow access to DynamoDB via a foreign key.
 * 
 * Run me with:
 *    ENV=production node dynamodb-parallel-scanner.js <workersCount>
 * Where <workersCount> is the number of parallel workers that will fetch and check items from the secondary table for each item from a separate slice of items.  Based on how slow DynamoDB is, a good value would be 128.  More than this is overkill and starts to be slower as the value increases.
 * 
 * Some tests I ran in a particular environment with 1,200,000+ items:

| Workers |     Items | Minutes | Items / min | ETA minutes |
-------------------------------------------------------------
|       8 |      8000 |    3:30 |        2290 |         567 |
|      32 |     38000 |       4 |        9500 |         136 |
|      64 |     38000 |       2 |       19000 |          69 |
|     128 |     38000 |    1:21 |       29230 |          44 |
|     256 |     38000 |    1:38 |       27140 |          48 |
|     128 | 1,220,000 |    1:21 |       29230 |          17 |

 * The last row contains the ACTUAL result - incredibly fast.

 * It'll first fetch all items that match the above criteria and put them into ...-STEP1.csv.
 * Then it'll split those items into separate, equally-sized slices, one slice for each worker.
 * Finally, it'll fork <workersCount> copies of itself, this time each child process will get and check secondary table items for its slice.
 * It waits for all child processes to end.
 * The result is put into ...-OUT.csv.
 */
const main = async () => {
  if (process.argv.length < 3) {
    console.error(`Please invoke me with node ${__filename} <workersCount>`)
    return
  }
  if (process.argv.length > 3) {
    if (process.argv.length < 5) {
      console.error(`Please invoke me with node ${__filename} <workerNumber> <start> <end>, where <start> and <end> are the boundaries of my slices`)
      return
    }
    await checkStep1ItemSlice(process.argv[2], {start: process.argv[3], end: process.argv[4]})
    return
  }

  await getStep1Items()
  await getStep2Items(process.argv[2])
}

main()
