
exports.logLevels = {
	err: 1,
	warn: 2,
	info: 3,
	debug: 4,
	trace: 5
};

configuredLevel = exports.logLevels.debug;

exports.error = function error(context, message, obj) {
	this.log(exports.logLevels.err, context, message, obj);
}

exports.warn = function warn(context, message, obj) {
	this.log(exports.logLevels.warn, context, message, obj);
}

exports.debug = function debug(context, message, obj) {
	this.log(exports.logLevels.debug, context, message, obj);
}

exports.log = function log(desiredLevel, context, message, obj) {
	message = `[${(new Date()).toISOString()}] [${context.constructor.name.toString()}] ${message}`;
	if (desiredLevel > this.configuredLevel) return;

	console.log(message, (obj == null || obj == undefined) ? "" : obj);
}
