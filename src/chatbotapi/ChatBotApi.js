var logService = require('./../chatmob/utils/log-service');
var ChatService = require('./../chatmob/utils/ChatService').ChatService;
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;
var botConnectorFactory = require('./bot-connector-factory').botConnectorFactory;

class ChatBotApi {

	constructor() {
		this.botConnector = botConnectorFactory.get(process.env.BOT_CONNECTOR_INSTANCE);
		this.botConnector.addListener(this.onBotMessage.bind(this));

		this.notifService = new PostNotifBotApiService(process.env.CHAT_NAME);
		this.notifService.addListener(this.onVisitorMessage.bind(this));

		this.chatService = new ChatService(process.env.CHAT_NAME, process.env.CHAT_POSTS_URL);
	}

	run() {
		this.notifService.register(process.env.CHAT_NOTIFICATIONS_URL)
	}

	onVisitorMessage(post) {
		if (post.name == process.env.CHAT_NAME) {
			return;
		}
		this.sendToConfiguredBot(post);
	}

	sendToConfiguredBot(post) {
		this.botConnector.send(post);
	}

	onBotMessage(message) {
		this.chatService.post(message);
	}

}

exports.ChatBotApi = ChatBotApi;
