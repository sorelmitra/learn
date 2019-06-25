var logService = require('../../chatmob/utils/log-service');
var EchoBot = require('../bots/EchoBot').EchoBot;

class BotConnectorEcho {

	constructor() {
		this.botMessagesListener = null;
		this.bot = new EchoBot();
		this.bot.addListener(this.onBotMessage.bind(this));
	}

	send(post) {
		this.bot.receive(post);
		logService.debug(this, `EchoBot < ${JSON.stringify(post)}`);
	}

	addListener(listener) {
		this.botMessagesListener = listener;
	}

	onBotMessage(data) {
		this.botMessagesListener(data);
	}
}

exports.default = BotConnectorEcho;
