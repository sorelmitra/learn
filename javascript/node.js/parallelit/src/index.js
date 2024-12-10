console.time('runtime')

process.env = {
	...process.env,
	AWS_SDK_LOAD_CONFIG: true,
	AWS_PROFILE: `${process.env.ENV}-profile`,
	AWS_NODEJS_CONNECTION_REUSE_ENABLED: '1'
}
if (!process.env.AWS_PROFILE) {
	process.env.AWS_PROFILE = `${process.env.ENV}-profile`
}

const fs = require('fs')
const { format } = require('@fast-csv/format')
const { parse } = require('@fast-csv/parse')
const { fork} = require('child_process')
const { Command } = require('commander')
const { getEscape } = require("./lib/para")
const program = new Command()

let step2LogInterval = 1000

const step1Filename = "./output/STEP1.csv"
const step2AllFilename = "./output/OUT.csv"
const getStep2Filename = number => `./output/STEP2-WORKER-${number}-OUT.csv`

const controllers = []

const exitHandler = (options, exitCode = 0) => {
	for (const c of controllers) {
		if (c) c.abort()
	}
	console.timeEnd("runtime");
	if (options.exit) process.exit(exitCode)
}

process.on('exit', exitHandler.bind(null, { exit: true }));
process.on('SIGINT', exitHandler.bind(null, { exit: true }));

const gatherRawData = async gathererFilepath => {
	const p = new Promise((resolve, reject) => {
		const child = fork(gathererFilepath, [step1Filename], {silent: true})
		child.stdout.on('data', (data) => {
		  process.stdout.write(`[Gatherer] ${data}`)
		})
		child.stderr.on('data', (data) => {
		  process.stderr.write(`[Gatherer] [ERROR] ${data}`)
		})
		child.on('close', (code) => {
			console.log(`[Gatherer] child process exited with code ${code}`)
			resolve()
		})
		child.on('error', err => reject(err))
	})
	await p
}

const getStep1ItemsCount = async () => new Promise(resolve => {
	fs.createReadStream(step1Filename)
		.pipe(parse({escape: getEscape()}))
		.on("error", error => console.error(error))
		.on('data', () => {
		})
		.on("end", (rowCount) => {
			let itemCount = rowCount - 1 // skip header row
			console.log(`${itemCount} items from step 1`)
			resolve(itemCount)
		})
});

const getSlices = (itemsCount, workersCount) => {
	if (workersCount < 1) return []
	const size = Math.floor(itemsCount / workersCount)
	const slices = []
	let start = 0
	let end = 0
	for (let i = 0; i < workersCount - 1; i++) {
		end = start + size
		slices.push({ start, end })
		start = end
	}
	slices.push({ start, end: itemsCount + 1 })
	console.log('Slices for workers', slices)
	return slices
}

const getWorkerResults = async (number, itemSlice) => new Promise(resolve => {
	let i = -2
	const results = []
	fs.createReadStream(getStep2Filename(number))
		.pipe(parse({escape: getEscape()}))
		.on("error", error => console.error(error))
		.on("data", async row => {
			i++
			if (i === -1) return // skip header row
			results.push(row)
		})
		.on("end", (rowCount) => {
			resolve(results)
		})
})

const gatherAllResults = async (slices, workerOutputFormat = "id,value") => {
	const stream = format({
		headers: workerOutputFormat.split(','),
		escape: getEscape()
	});
	stream.pipe(fs.createWriteStream(step2AllFilename));
	for (let i = 0; i < slices.length; i++) {
		const results = await getWorkerResults(i + 1);
		for (const result of results) {
			stream.write(result)
		}
	}
	stream.end()
}

const startAWorker = (workerFilepath, workerIndex, slice, timeout) => new Promise(resolve => {
	const number = workerIndex + 1
	controllers[workerIndex] = new AbortController()
	const {signal} = controllers[workerIndex]
	let child
	setTimeout(() => {
		child = fork(
			workerFilepath,
			[
				'work',
				'-n', number,
				'-s', slice.start, '-e', slice.end,
				'-o', getStep2Filename(number),
				'-i', step2LogInterval,
				step1Filename
			],
			{signal, silent: true}
		)
		child.stdout.on('data', (data) => {
		  process.stdout.write(`[Worker ${number}] ${data}`)
		})
		child.stderr.on('data', (data) => {
		  process.stderr.write(`[Worker ${number}] [ERROR] ${data}`)
		})
		child.on('close', (code) => {
			console.log(`[Worker ${number}] child process exited with code ${code}`);
			resolve()
		})
	}, timeout)
})

const startWorkers = async (workersCount, workerFilepath, workerOutputFormat) => {
	const n = await getStep1ItemsCount()

	const slices = getSlices(n, workersCount)
	const children = []
	for (let i = 0; i < slices.length; i++) {
		const slice = slices[i];
		children[i] = startAWorker(workerFilepath, i, slice, 2000 * i)
	}
	await Promise.all(children)
	await gatherAllResults(slices, workerOutputFormat)
	console.log(`All ${children.length} children have finished`)
}

const main = async () => {
	program
		.name('parallelit')
		.version('1.0.0')
		.description(`
Process arbitrary data in parallel, using Node.JS fork, in two steps: first launch a single instance of a Node.JS file that will gather the raw data to process.  Then split the resulting CSV file into N slices, which are fed into N instances of another Node.JS file.  Finally, gather and report results.

Run like:
node index.js [--gatherer <gather-raw-data-JS>] --count <workersCount> --worker <worker-JS>

For more information please see the readme:
https://github.com/sorelmitra/parallelit
  `)

	program.command('run')
		.description('Run parallel workers on slices of data')
		.requiredOption('-c, --count <number>', `Number of parallel workers to be spawned`, '1')
		.option('-w, --worker <file path>', 'Path to the Node.JS worker file')
		.option('-g, --gatherer <file path>', "Path to the Node.JS gatherer file.  If not specified, it will not gather data, and will assume it's been already gathered")
		.option('-i, --log-interval <number>', 'Number of loans after which to log status')
		.option('-f, --worker-output-format <number>', 'Format of the output CSV file created by workers, e.g. "id,value"')
		.action(async (options) => {
			if (options.logInterval) step2LogInterval = options.logInterval
			try {
				if (options.gatherer) await gatherRawData(options.gatherer)
				if (options.worker) await startWorkers(options.count, options.worker, options.workerOutputFormat)
				if (!options.worker && !options.gatherer) console.log('Nothing to do, please use a worker or gatherer in order to do some meaningful work.')
			} catch (e) {
				console.error(e)
			}
		})

	await program.parseAsync()
}

main().then(() => console.log('Done.')).catch(e => console.error(e)).finally(() => console.timeEnd('runtime'))
