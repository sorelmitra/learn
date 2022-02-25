/* eslint-disable */
/* eslint-disable prettier/prettier */

const fs = require('fs')
const { CognitoIdentityServiceProvider } = require('aws-sdk');
const { parse } = require('@fast-csv/parse')
const { Command } = require('commander');

const program = new Command()

const env = process.env.ENV
if (!env) {
	console.log('Please define ENV!')
	process.exit(1)
} else if (['prod', 'production'].includes(env.toLowerCase())) {
	console.log('Refusing to work in production!')
	process.exit(2)
} else if (['stage', 'sandbox'].includes(env.toLowerCase())) {
	console.log('Refusing to work in pre-production!')
	process.exit(2)
}

process.env = {
	...process.env,
	AWS_SDK_LOAD_CONFIG: true,
	AWS_PROFILE: `${process.env.ENV}-profile`,
	AWS_NODEJS_CONNECTION_REUSE_ENABLED: '1'
}

const cognito = new CognitoIdentityServiceProvider();

const checkUserPoolExists = async poolName => {
	const data = await cognito.listUserPools({ MaxResults: 60 }).promise()
	let poolId = undefined
	const result = data.UserPools?.map(async pool => {
		if (pool.Name !== poolName) { return }
		if (!pool.Id) {
			console.warn(`Cognito user pool ${poolName} has no ID!`)
		}
		if (poolId) {
			console.warn(`More than one pools with name ${poolName} exist: ${poolId}`)
		}
		poolId = pool.Id
	})
	if (result) {
		await Promise.all(result)
	}
	return Promise.resolve(poolId)
}

const createUserPool = async poolName => {
	console.log(`START Create user pool ${poolName}`)
	let poolId = await checkUserPoolExists(poolName)
	if (poolId) {
		console.log(`Pool already exists, nothing to do`)
	} else {
		const data = await cognito.createUserPool({ PoolName: poolName }).promise()
		poolId = data.UserPool?.Id
		console.log(`Created pool`)
	}
	console.log(`DONE Create Cognito user pool ${poolName}`, poolId)
	return Promise.resolve()
}

function LinkedList() {
	this.value = undefined
	this.next = null
	return this
}

const getUsersCount = async (filename) => {
	return new Promise(resolve => {
		fs.createReadStream(filename)
			.pipe(parse())
			.on("error", error => console.error(error))
			.on('data', () => { })
			.on("end", (rowCount) => {
				let itemCount = rowCount - 1 // skip header row
				console.log(`${itemCount} users`)
				resolve(itemCount)
			})
	})
}

const readUsers = async (filename, start, end) => new Promise(resolve => {
	let i = -1
	const itemSlice = new LinkedList(null, null)
	let current = itemSlice
	fs.createReadStream(filename)
		.pipe(parse())
		.on("error", error => console.error(error))
		.on("data", async row => {
			if (row[0] == 'poolId') return // skip header row
			i++
			if (i < start || i >= end) return;
			current.value = row
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

const createUser = async user => {
	console.log('START Create user ', user)
	if (await cognito.adminGetUser({
			UserPoolId: user[0],
			Username: user[1]
		}).promise()) {
		console.log(`User already exists`)
		return Promise.resolve()
	}
	await cognito.adminCreateUser({
		UserPoolId: user[0],
		Username: user[1],
		UserAttributes: [
			{ Name: "given_name", Value: user[2] },
			{ Name: "family_name", Value: user[3] },
			{ Name: "email", Value: user[4] }
		],
		TemporaryPassword: 'TiPsdwok1!'
	}).promise()
	console.log('DONE Create user ', user)
	return Promise.resolve()
}

const createUsers = async filename => {
	const count = await getUsersCount(filename)
	const slice = { start: 0, end: count }
	const users = await readUsers(filename)
	for (let current = users; current.next != null; current = current.next) {
		const user = current.value
		await createUser(user)
	}
	return Promise.resolve()
}

const main = async () => {
	program
		.name('cognito-manager')
		.version('0.1.0')
		.description(`
Manage AWS Cognito Stuff.  WIP.`)

	program.command('userpool')
		.description('Commands on user pools')
		.argument('<pool-name>', `Name of the user pool.`)
		.option('-c, --create', 'Create the user pool.')
		.action(async (name, options) => {
			const poolName = `${env}-${name}`
			if (options.create) {
				await createUserPool(poolName)
			} else {
				console.log(`User pool ${poolName}, but not instructed what to do with it, use options for that.`)
			}
		})

	program.command('users')
		.description('Commands on users')
		.argument('<users-file-path>', `CSV file with the users to operate on.`)
		.option('-c, --create', 'Create the users.')
		.action(async (filename, options) => {
			if (options.create) {
				await createUsers(filename)
			} else {
				console.log(`Users file ${filename}, but not instructed what to do with it, use options for that.`)
			}
		})

	await program.parseAsync()
}

main().then(() => console.log('Done.')).catch(e => console.error(e))
