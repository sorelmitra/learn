import logService from './utils/log-service';
import Config from 'react-native-config';
import postNotifService from './postnotif-service';

class ChatService {

	async post(message) {
		var data = {
			name: Config.CHAT_NAME,
			body: message
		};
		const resp = await this.restJsonCall('POST', Config.CHAT_POSTS_URL, data);
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

	addPostNotificationListener(listener) {
		postNotifService.addListener(listener);
		postNotifService.register(Config.CHAT_NOTIFICATIONS_URL);
	}

	async restJsonCall(restMethod, url, data) {
		logService.debug(this, `Launching ${restMethod} to <${url}>: <${data.body}>`);
		const response = await fetch(url, {
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
