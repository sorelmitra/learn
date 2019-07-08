var logService = require('../../chatmob/utils/log-service');

class EchoBot {

	constructor() {
		this.outgoingListener = null;
	}

	addListener(listener) {
		this.outgoingListener = listener;
	}

	receive(post) {
		this.respond(post.body);
	}

	respond(message) {
		let data = this.getResponseData(message);
		this.outgoingListener(data);
	}

	getResponseData(message) {
		let data = {
			body: `EchoBot is here: You said "${message}", thank you`
		}
		return data;
	}
}

exports.default = EchoBot;
