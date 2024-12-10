const { Command } = require('commander')
const fs = require("fs");
const {parse} = require("@fast-csv/parse");
const program = new Command()

module.exports.periodicReport = (label, index, logInterval, count, extra = '') => {
  if (index && index % logInterval === 0) {
    console.timeLog(label, `At item ${index}/${count}${extra}...`)
  }
}

module.exports.gathererMain = async gatherFunc => {
  program
    .description(`Gather raw data for parallel processing`)
    .argument('<output-filepath>', 'CSV raw data to where to put gathered raw data')
    .option('-i, --log-interval <number>', 'After how many items to report progress at the console')
 		.action(async (outputFilepath, options) => {
 			await gatherFunc(outputFilepath, options.logInterval)
 		})

  await program.parseAsync()
}

module.exports.getEscape = () => '\\'

module.exports.getSlice = (source, start, end, header = '') => new Promise(resolve => {
  let i = -2
  const slice = []
  fs.createReadStream(source)
    .pipe(parse({escape: module.exports.getEscape()}))
    .on("error", error => console.error(error))
    .on("data", async row => {
      i++
      if (i === -1) return // skip header row
      if (i < start || i >= end) return // not my slice
      slice.push(row)
    })
    .on("end", (rowCount) => {
      console.timeLog('runtime', `${header}Read slice [${start}, ${end}) from ${source}`)
      resolve(slice)
    })
})

module.exports.workerMain = async doWorkFunc => {
  program.command('work')
 		.description(`Work on a slice from a CSV file`)
 		.argument('<slice-source>', 'CSV raw data to where my slice is located')
 		.requiredOption('-n, --number <number>', 'Current worker number, used when creating the output file and when reporting progress')
 		.requiredOption('-s, --start <number>', 'First non-header line number from <slice-source> to process')
    .requiredOption('-e, --end <number>', 'Last non-header line number from <slice-source> to process')
    .requiredOption('-o, --output <filepath>', 'File path where to output the results')
 		.option('-i, --log-interval <number>', 'After how many items to report progress at the console')
 		.action(async (sliceSource, options) => {
 			await doWorkFunc(options.number, {start: options.start, end: options.end}, sliceSource, options.output, options.logInterval)
 		})

  await program.parseAsync()
}
