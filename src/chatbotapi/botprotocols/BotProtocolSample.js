
class BotProtocolSample {

	constructor() {

	}

	buildRegistrationMessage() {
		let o = {
			name: process.env.NAME_FOR_BOT,
			command: 'register'
		}
		return JSON.stringify(o);
	}

	receiveMessage(data) {
		response = this.getResponse(data);
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
		if (reg.name != this.chatName) {
			logService.error(this, `Error: Received out-of-band registration response for name '${reg.name}', our name is '${this.chatName}'`)
			return false;
		}
		
		logService.debug(this, `Registered as a chat bot client with ID ${reg.id}`);
		return true;
	}

}

exports.default = BotProtocolSample;
