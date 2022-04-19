const { Command } = require('commander');
const program = new Command();

program
	.name('string-util')
	.description('CLI to some JavaScript string utilities')
	.version('0.8.0');

program.command('split')
	.description('Split a string into substrings and display as an array')
	.argument('<string>', 'string to split')
	.option('--first', 'display just the first substring')
	.option('-s, --separator <char>', 'separator character', ',')
	.action((str, options) => {
		const limit = options.first ? 1 : undefined;
		console.log(str.split(options.separator, limit));
	});

program.command('join')
	.description('Join an array into a string')
	.argument('<array,>', 'array to join')
	.option('-s, --slice <start,end>', 'join just the given slice')
	.option('-j, --joiner <char>', 'character to join with', ' ')
	.action((str, options) => {
		const arr = str.split(',');
		const sl = options.slice.split(',');
		const [ start, end ] = options.slice ? sl : [0];
		console.log(arr.slice(start, end).join(options.joiner));
	});

program.parse();
