var logService = require('./../../chatmob/utils/log-service');

class BotConnectorDummy {

	constructor() {
		this.botMessagesListener = null;
	}

	send(post) {
		logService.debug(this, `Bot on the way; ${JSON.stringify(post)}`);
	}

	addListener(listener) {
		this.botMessagesListener = listener;
	}
}

exports.default = BotConnectorDummy;
