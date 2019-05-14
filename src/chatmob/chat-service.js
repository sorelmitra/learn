import logService from './log-service';

class ChatService {

	post(message) {
		logService.debug(this, "Posting TODO");
	}
}

export default chatService = new ChatService();
