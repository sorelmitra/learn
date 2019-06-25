var logService = require('./log-service');

class ChatService {

	constructor(fetchCb, chatName, chatPostsUrl) {
		this.chatName = chatName;
		this.chatPostsUrl = chatPostsUrl;
		this.fetchCb = fetchCb;
	}

	async post(message) {
		var data = {
			name: this.chatName,
			body: message
		};
		const resp = await this.restJsonCall('POST', this.chatPostsUrl, data);
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

	async restJsonCall(restMethod, url, data) {
		logService.debug(this, `Launching ${restMethod} to <${url}>: <${data.body}>`);
		const response = await this.fetchCb(url, {
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

exports.ChatService = ChatService;
