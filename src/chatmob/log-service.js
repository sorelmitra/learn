
export var logLevels = {
	err: 1,
	warn: 2,
	info: 3,
	debug: 4,
	trace: 5
};

class LogService {

	configuredLevel = logLevels.debug;

	debug(context, message) {
		this.log(logLevels.debug, context, message);
	}

	log(desiredLevel, context, message) {
		message = "[" + context.constructor.name.toString() + "] " + message;
		if (desiredLevel > this.configuredLevel) return;

		console.log(message);
	}
}

export default logService = new LogService();
