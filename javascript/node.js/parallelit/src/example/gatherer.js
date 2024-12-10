console.time('runtime')

const fs = require("fs")
const {format} = require("@fast-csv/format")

const {gathererMain, periodicReport} = require("../lib/para")

const gather = async (outputFilepath, logInterval) => {
  logInterval = logInterval ?? 1000
  const stream = format({ headers: ['id', 'value'] })
 	stream.pipe(fs.createWriteStream(outputFilepath))

  const count = 163
  for (let i = 1; i < count + 1; i++) {
    periodicReport('runtime', i, logInterval, count);
    const randomNumber = i + Math.floor(Math.random() + 20)
    stream.write([i, randomNumber])
  }

  stream.end()
}

gathererMain(gather).then(() => console.log('Done.')).catch(e => console.error(e)).finally(() => console.timeEnd('runtime'))
