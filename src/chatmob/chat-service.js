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
		fetch(this.chatPostUrl, {
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
		.then(response => response.json())
		.then(resp => {
			logService.debug(this, `Posted, got response <${JSON.stringify(resp)}>`);
		});
	}
}

export default chatService = new ChatService();
