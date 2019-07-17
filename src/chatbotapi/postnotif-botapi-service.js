var logService = require('./../chatmob/utils/log-service');
var ChatApiNotif = require('./../chatmob/utils/ChatApiNotif').ChatApiNotif;
var WebSocketClient = require('websocket').client;

class PostNotifBotApiService {

	constructor(name) {
		this.notif = new ChatApiNotif(name);
		this.registered = false;
	}

	register(url) {
		let client = new WebSocketClient();
		client.on('connect', (connection) => {
			let data = this.notif.buildRegistrationMessage();
			connection.sendUTF(data);
			logService.debug(this, `> ${data}`);

			connection.on('message', (message) => {
				let data = message.utf8Data;
				logService.debug(this, `< ${data}`);
				if (this.registered) {
					this.notif.receiveMessage(data);
					return;
				}
				this.registered = this.notif.receiveRegistrationResponse(data);
			});

			connection.on('close', (e) => {
				logService.debug(this, `Connection closed: ${e}`);
				this.registered = false;
			});

			connection.on('error', (e) => {
				logService.error(this, `Connection error: ${e}`);
			});
		});

		client.on('connectFailed', (e) => {
			logService.error(this, `Connection error: ${e}`);
		});

		client.connect(url);
	}

	addListener(listener) {
		this.notif.addListener(listener);
	}

}

exports.PostNotifBotApiService = PostNotifBotApiService;
