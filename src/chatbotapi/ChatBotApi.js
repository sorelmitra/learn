var config = require('./../chatmob/config/config').config;
var logService = require('./../chatmob/utils/log-service');
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;

class ChatBotApi {

	constructor() {
		//this.botConnector = botConnectorFactory.get(config.bots[config.bot]);
		this.notifService = new PostNotifBotApiService(config.chat.name);
		this.notifService.addListener(this.onIncomingMessage);		
	}

	getPostsUrl() {
		return config.chat.posts.host + config.chat.posts.path;
	}

	getNotificationsUrl() {
		return config.chat.notifications.host + config.chat.notifications.path;
	}

	run() {
		this.notifService.register(this.getNotificationsUrl())
	}

	onIncomingMessage(data) {
		logService.debug(`< ${data}`)
		let o = JSON.parse(data);
		let post = o['post']
		if (post == undefined) {
			logService.warn(`Incoming message has no 'post' field, discarding`);
			return;
		}
		sendToConfiguredBot(post);
	}

	sendToConfiguredBot(post) {
		//this.botConnector.send(post);
		logService.debug(`Will be sending to bot soon`);
	}

}

exports.ChatBotApi = ChatBotApi;
