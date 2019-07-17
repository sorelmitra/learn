var logService = require('../../chatmob/utils/log-service');

class BotProtocolSample {

	constructor() {
		this.nameForBot = process.env.NAME_FOR_BOT;
	}

	buildRegistrationMessage() {
		let o = {
			name: this.nameForBot,
			command: 'register'
		}
		return JSON.stringify(o);
	}

	receiveMessage(data) {
		let response = this.getResponse(data);
		return response;
	}

	getResponse(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Chat bot response error: ${o.reason}`);
			return null;
		}
		return o.botResponse;
	}

	receiveRegistrationResponse(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Error registering as a chat bot client: ${o.reason}`);
			return false;
		}

		let reg = o["client-registration"];
		if (reg.name != this.nameForBot) {
			logService.error(this, `Error: Received out-of-band registration response for name '${reg.name}', our name is '${this.nameForBot}'`)
			return false;
		}
		
		logService.debug(this, `Registered as a chat bot client`);
		return true;
	}

}

exports.default = BotProtocolSample;
