var config = require('./../chatmob/config/config').config;
var logService = require('./../chatmob/utils/log-service');
var PostNotifBotApiService = require('./postnotif-botapi-service').PostNotifBotApiService;

class ChatBotApi {

	constructor() {
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
	}

}

exports.ChatBotApi = ChatBotApi;
