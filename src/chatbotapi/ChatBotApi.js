var logService = require('./../chatmob/utils/log-service');
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;
var botConnectorFactory = require('./bot-connector-factory').botConnectorFactory;

class ChatBotApi {

	constructor() {
		this.botConnector = botConnectorFactory.get(process.env.BOT_CONNECTOR_INSTANCE);
		this.notifService = new PostNotifBotApiService(process.env.CHAT_NAME);
		this.notifService.addListener(this.onIncomingMessage.bind(this));		
	}

	run() {
		this.notifService.register(process.env.CHAT_NOTIFICATIONS_URL)
	}

	onIncomingMessage(post) {
		this.sendToConfiguredBot(post);
	}

	sendToConfiguredBot(post) {
		this.botConnector.send(post);
	}

}

exports.ChatBotApi = ChatBotApi;
