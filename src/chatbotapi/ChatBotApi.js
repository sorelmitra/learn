var logService = require('./../chatmob/utils/log-service');
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;

class ChatBotApi {

	constructor() {
		//this.botConnector = botConnectorFactory.get(config.bots[config.bot]);
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
		//this.botConnector.send(post);
		logService.debug(this, `Will be sending post ${post.id} = ${post.body} to bot soon`);
	}

}

exports.ChatBotApi = ChatBotApi;
