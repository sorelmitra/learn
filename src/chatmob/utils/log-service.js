
exports.logLevels = {
	err: 1,
	warn: 2,
	info: 3,
	debug: 4,
	trace: 5
};

configuredLevel = exports.logLevels.debug;

exports.error = function error(context, message) {
	this.log(exports.logLevels.err, context, message);
}

exports.warn = function warn(context, message) {
	this.log(exports.logLevels.warn, context, message);
}

exports.debug = function debug(context, message) {
	this.log(exports.logLevels.debug, context, message);
}

exports.log = function log(desiredLevel, context, message) {
	message = `[${(new Date()).toISOString()}] [${context.constructor.name.toString()}] ${message}`;
	if (desiredLevel > this.configuredLevel) return;

	console.log(message);
}
