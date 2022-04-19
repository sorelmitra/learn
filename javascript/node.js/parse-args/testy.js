const yargs = require('yargs');

const argv = yargs
	.command('lyr', 'Tells whether an year is leap year or not', {
		year: {
			description: 'the year to check for',
			alias: 'y',
			type: 'number'
		}
	})
	.command('mlen', 'Tells whether the month is long', {
		month: {
			description: 'the month to check for',
			alias: 'm',
			type: 'number'
		}
	})
	.option('time', {
		alias: 't',
		description: 'Tell the present Time',
		type: 'boolean'
	})
	.help()
	.alias('help', 'h').argv;

if (argv.time) {
	console.log('The current time is: ', new Date().toLocaleTimeString());
}

if (argv._.includes('lyr')) {
	const year = argv.year || new Date().getFullYear();
	if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) {
		console.log(`${year} is a Leap Year`);
	} else {
		console.log(`${year} is NOT a Leap Year`);
	}
}

if (argv._.includes('mlen')) {
	const month = argv.month || new Date().getMonth();
	if ([0, 2, 4, 6, 7, 9, 11].includes(month)) {
		console.log(`${month} is long`);
	} else {
		console.log(`${month} is shorter`);
	}
}
