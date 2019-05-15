import logService from './log-service';
import config from './config';

class ChatService {

	chatPostUrl = "" + config.chat.host + config.chat.path.post;

	post(message) {
		logService.debug(this, `Posting to <${this.chatPostUrl}>: <${message}>`);
		var data = {
			title: 'foo',
			body: message,
			userId: 1
		};
		return fetch(this.chatPostUrl, {
			method: 'POST',
			mode: 'cors',
			cache: 'no-cache',
			credentials: 'same-origin',
			headers: {
				'Content-Type': 'application/json',
			},
			redirect: 'follow',
			referrer: 'no-referrer',
			body: JSON.stringify(data), // body data type must match "Content-Type" header
		})
		.then(response => response.json());
	}
}

export default chatService = new ChatService();
