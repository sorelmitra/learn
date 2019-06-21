var logService = require('./log-service');

exports.ChatApiNotif = function ChatApiNotif(name) {
	this.chatName = name;
	this.listener = null;

	this.addListener = function addListener(listener) {
		this.listener = listener;
	}

	this.receiveRegistrationResponse = function receiveRegistrationResponse(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Error registering for post notifications: ${o.reason}`);
			return false;
		}

		let reg = o["notification-registration"];
		if (reg.name != this.chatName) {
			logService.error(this, `Error: Received out-of-band registration response for name '${reg.name}', our name is '${this.chatName}'`)
			return false;
		}
		
		logService.debug(this, `Registered for post notifications with ID ${reg.id}`)
		return true;
	}

	this.receiveMessage = function receiveMessage(data) {
		if (this.listener == null) {
			logService.debug(this, `No Listener configured`);
			return;
		}
		post = this.getPost(data);
		if (post == null) {
			return;
		}
		if (this.chatName == post.name) {
			return
		}
		this.listener(post.body);
	}
	
	this.getPost = function getPost(data) {
		let o = JSON.parse(data);
		if (!o.success) {
			logService.error(this, `Post notification error: ${o.reason}`);
			return null;
		}
		return o.post;
	}

	this.buildRegistrationMessage = function buildRegistrationMessage() {
		let o = {
			name: this.chatName,
			command: 'register'
		}
		return JSON.stringify(o);
	}

	return this;
}
