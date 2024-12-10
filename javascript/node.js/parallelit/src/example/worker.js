console.time('runtime')

const fs = require("fs")
const {format} = require("@fast-csv/format")

const {workerMain, getSlice, periodicReport} = require("../lib/para")

const doWork = async (number, slice, sliceSource, outputFilepath, logInterval) => {
  logInterval = logInterval ?? 1000
  const sliceItems = await getSlice(sliceSource, slice.start, slice.end)
  const stream = format({ headers: ['id', 'value'] })
 	stream.pipe(fs.createWriteStream(outputFilepath))

  const simulateHardWork = async value => new Promise(resolve => {
    setTimeout(() => {
      resolve(value * 2)
    }, 1000 * Math.random())
  })

  const count = sliceItems.length
  for (let i = 0; i < count; i++) {
    const item = sliceItems[i]
    periodicReport('runtime', i, logInterval, count, ' (0 errors) ')
    const doubledInt = await simulateHardWork(item[1])
    stream.write([item[0], doubledInt])
  }

  stream.end()
}

workerMain(doWork).then(() => console.log('Done.')).catch(e => console.error(e)).finally(() => console.timeEnd('runtime'))
