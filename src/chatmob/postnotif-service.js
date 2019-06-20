import logService from './log-service';
import config from './config';

class PostNotifService {

	constructor() {
		this.chatName = config.chat.name;
		this.listener = null;
		this.registered = false;
	}

	register(url) {
		let ws = new WebSocket(url);
		ws.onopen = () => {
			data = this.buildRegistrationMessage();
			ws.send(data);
			logService.debug(this, `> ${data}`);
		};

		ws.onmessage = (e) => {
			logService.debug(this, `< ${e.data}`);
			if (this.registered) {
				this.receiveMessage(e.data);
				return;
			}
			this.receiveRegistrationResponse(e.data);
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
		this.listener = listener;
	}

	receiveRegistrationResponse(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Error registering for post notifications: ${o.reason}`);
			return;
		}

		let reg = o["notification-registration"];
		if (reg.name != this.chatName) {
			logService.error(this, `Error: Received out-of-band registration response for name '${reg.name}', our name is '${this.chatName}'`)
			return;
		}
		
		logService.debug(this, `Registered for post notifications with ID ${reg.id}`)
		this.registered = true;
		return;
	}

	receiveMessage(data) {
		if (this.listener == null) {
			logService.debug(this, `No Listener configured`);
			return;
		}
		post = this.getMessage(data);
		if (post == null) {
			return;
		}
		if (this.chatName == post.name) {
			return
		}
		this.listener(post.body);
	}
	
	getMessage(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Post notification error: ${o.reason}`);
			return null;
		}
		return o.post;
	}

	buildRegistrationMessage() {
		let o = {
			name: this.chatName,
			command: 'register'
		}
		return JSON.stringify(o);
	}
}

export default postNotifService = new PostNotifService();
