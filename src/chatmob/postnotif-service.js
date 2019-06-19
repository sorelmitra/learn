import logService from './log-service';

class PostNotifService {

	constructor() {
		this.postNotifRegistreeName = 'BotAgg Chat Mobile';
		this.listener = null;
		this.registered = false;
	}

	register(url) {
		let ws = new WebSocket(url);
		ws.onopen = () => {
			ws.send(this.buildRegistrationMessage());
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
		if (reg.name != this.postNotifRegistreeName) {
			logService.error(this, `Error: Received out-of-band registration response for name '${reg.name}', our name is '${this.postNotifRegistreeName}'`)
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
		message = this.getMessage(data);
		if (message != null) {
			this.listener(message);
		}
	}
	
	getMessage(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Post notification error: ${o.reason}`);
			return null;
		}
		return o.post.body;
	}

	buildRegistrationMessage() {
		let o = {
			name: this.postNotifRegistreeName,
			command: 'register'
		}
		return JSON.stringify(o);
	}
}

export default postNotifService = new PostNotifService();
