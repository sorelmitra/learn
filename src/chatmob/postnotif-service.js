import logService from './utils/log-service';
import Config from 'react-native-config';
var ChatApiNotif = require('./../chatmob/utils/ChatApiNotif').ChatApiNotif;

class PostNotifService {

	constructor() {
		this.notif = new ChatApiNotif(Config.CHAT_NAME);
		this.registered = false;
	}

	register(url) {
		let ws = new WebSocket(url);
		ws.onopen = () => {
			let data = this.notif.buildRegistrationMessage();
			ws.send(data);
			logService.debug(this, `> ${data}`);
		};

		ws.onmessage = (e) => {
			logService.debug(this, `< ${e.data}`);
			if (this.registered) {
				this.notif.receiveMessage(e.data);
				return;
			}
			this.registered = this.notif.receiveRegistrationResponse(e.data);
		};

		ws.onerror = (e) => {
			logService.error(this, `Connection error: ${e.message}`);
		};

		ws.onclose = (e) => {
			logService.debug(this, `Connection closed with code ${e.code}, ${e.reason}`);
			this.registered = false;
		};
	}

	addListener(listener) {
		this.notif.addListener(listener);
	}

}

export default postNotifService = new PostNotifService();
