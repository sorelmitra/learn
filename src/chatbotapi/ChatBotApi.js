var logService = require('./../chatmob/utils/log-service');
const fetch = require("node-fetch");
var ChatService = require('./../chatmob/utils/ChatService').ChatService;
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;
var botConnectorFactory = require('./bot-connector-factory').botConnectorFactory;

class ChatBotApi {

	constructor() {
		this.botConnector = botConnectorFactory.get(process.env.BOT_CONNECTOR_INSTANCE);
		this.botConnector.addListener(this.onBotMessage.bind(this));

		this.notifService = new PostNotifBotApiService(process.env.CHAT_NAME);
		this.notifService.addListener(this.onVisitorMessage.bind(this));

		this.chatService = new ChatService(fetch, process.env.CHAT_NAME, process.env.CHAT_POSTS_URL);
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

	onBotMessage(data) {
		let message = data.body;
		let self = this;
		let s = JSON.stringify(data);
		this.chatService.post(message)
		.then(function(resp) {
			logService.debug(self, `> ${s}`);
		})
		.catch(function(error) {
			logService.error(self, `Could not send ${s}: ${error}`);
		});
	}

}

exports.ChatBotApi = ChatBotApi;
