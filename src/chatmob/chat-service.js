import logService from './log-service';
import config from './config';

class ChatService {

	async post(message) {
		var data = {
			body: message
		};
		const resp = await this.restJsonCall('POST', config.chat.path.posts, data);
		return new Promise(function(resolve, reject) {
			if (resp.success) {
				if (resp.post.body == data.body) {
					resolve(resp.post);
				} else {
					throw `Response from server is 'success' but for another message: <${resp.body}>`;
				}
			} else {
				if (resp.reason === undefined) {
					throw "Unknown response from server!";
				}
				throw resp.reason;
			}
		});
	}

	async restJsonCall(restMethod, path, data) {
		chatPostUrl = "" + config.chat.host + path;
		logService.debug(this, `Launching ${restMethod} to <${chatPostUrl}>: <${data.body}>`);
		const response = await fetch(chatPostUrl, {
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
