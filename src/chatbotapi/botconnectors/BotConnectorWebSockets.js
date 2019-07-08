var logService = require('../../chatmob/utils/log-service');
var WebSocketClient = require('websocket').client;

class BotConnectorWebSockets {

	constructor(uri, protocolHandler) {
		this.uri = uri;
		this.protocolHandler = protocolHandler;
		this.botMessagesListener = null;
		this.registered = false;
		this.connection = null;
	}
	
	register() {
		let client = new WebSocketClient();
		client.on('connect', (connection) => {
			this.connection = connection;
			let data = this.protocolHandler.buildRegistrationMessage();
			this.connection.sendUTF(data);
			logService.debug(this, `> ${data}`);

			this.connection.on('message', (message) => {
				let data = message.utf8Data;
				logService.debug(this, `< ${data}`);
				if (this.registered) {
					let response = this.protocolHandler.receiveMessage(data);
					this.botMessagesListener(response);
					return;
				}
				this.registered = this.protocolHandler.receiveRegistrationResponse(data);
			});

			this.connection.on('close', (e) => {
				logService.debug(this, `Connection closed: ${e}`);
				this.registered = false;
			});

			this.connection.on('error', (e) => {
				logService.error(this, `Connection error: ${e}`);
			});
		});

		client.on('connectFailed', (e) => {
			logService.error(this, `Connection error: ${e}`);
		});

		client.connect(this.uri);
	}

	send(post) {
		if (this.connection == null) {
			logService.error(this, `Can't send: no connection`);
			return;
		}
		this.connection.send(post);
		logService.debug(this, `WebSockets Bot Connector < ${JSON.stringify(post)}`);
	}

	addListener(listener) {
		this.botMessagesListener = listener;
	}

}

exports.default = BotConnectorWebSockets;
