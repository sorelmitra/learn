var logService = require('./../../chatmob/utils/log-service');

class BotConnectorDummy {
	send(post) {
		logService.debug(this, `Bot on the way; ${JSON.stringify(post)}`);
	}
}

exports.default = BotConnectorDummy;
