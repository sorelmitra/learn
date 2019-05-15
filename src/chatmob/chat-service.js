import logService from './log-service';
import config from './config';

class ChatService {

	chatPostUrl = "" + config.chat.host + config.chat.path.post;

	async post(message) {
		logService.debug(this, `Posting to <${this.chatPostUrl}>: <${message}>`);
		var data = {
			body: message
		};
		const resp = await this.restJsonCall('POST', data);
		return new Promise(function() {
			if (resp.success) {
				resolve();
			} else {
				if (resp.reason === undefined) {
					throw "Unknown response from server!";
				}
				throw resp.reason;
			}
		});
	}

	async restJsonCall(restMethod, data) {
		const response = await fetch(this.chatPostUrl, {
			method: restMethod,
			mode: 'cors',
			cache: 'no-cache',
			credentials: 'same-origin',
			headers: {
				'Content-Type': 'application/json',
			},
			redirect: 'follow',
			referrer: 'no-referrer',
			body: JSON.stringify(data),
		});
		return await response.json();
	}
}

export default chatService = new ChatService();
