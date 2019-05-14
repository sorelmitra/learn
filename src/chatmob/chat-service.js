import logService from './log-service';
import config from './config';

class ChatService {

	chatPostUrl = "" + config.chat.host + config.chat.path.post;

	post(message) {
		logService.debug(this, `Posting to <${this.chatPostUrl}>: <${message}>`);
	}
}

export default chatService = new ChatService();
