var logService = require('./../chatmob/utils/log-service');
var BotConnectorWebSockets = require('./botconnectors/BotConnectorWebSockets').default;

class BotConnectorFactory {

	get(botConnectorInstance) {
		let botConnectorType = process.env[`BOT_CONNECTOR_${botConnectorInstance}_TYPE`];
		let botProtocolUri = process.env[`BOT_CONNECTOR_${botConnectorInstance}_PROTOCOL_HANDLER`];
		let botConnectorUri = process.env[`BOT_CONNECTOR_${botConnectorInstance}_URI`];
		return this.getByTypeAndUri(botConnectorType, botProtocolUri, botConnectorUri);
	}

	getByTypeAndUri(botConnectorType, botProtocolUri, botConnectorUri) {
		let botProtocolHandler = this.createObjectFromUri(botProtocolUri);
		if (botConnectorType == "javascript-class") {
			return this.createObjectFromUri(botConnectorUri);
		} else if (botConnectorType == "websockets") {
			return this.getWebSocketsBotConnector(botProtocolHandler, botConnectorUri);
		}
		throw `Unknown bot type ${botConnectorType}`; 
	}

	createObjectFromUri(objectUri) {
		if (objectUri == "none") {
			logService.debug(this, `No URI specified, nothing to create`);
			return null;
		}
		let uri = this.getUri(objectUri);
		if (uri.protocol != 'file') {
			throw `Unsupported uri '${uri.protocol}' for loading a class`;
		}
		let BotClass = require(uri.path).default;
		let bot = new BotClass();
		logService.debug(this, `Created object of class '${uri.path}'`);
		return bot;
	}

	getWebSocketsBotConnector(botProtocolHandler, botConnectorUri) {
		let uri = this.getUri(botConnectorUri);
		if (uri.protocol != 'ws') {
			throw `Unsupported protocol ${uri.protocol} for websockets bot`;
		}
		let botConnector = new BotConnectorWebSockets(botConnectorUri, botProtocolHandler);
		botConnector.register();
		return botConnector;
	}

	getUri(uri) {
		let re = /(\w+):\/\/(.*)/;
		let results = re.exec(uri);
		if (results == null) {
			return null;
		}
		return {
			protocol: results[1],
			path: results[2]
		}
	}
}

exports.botConnectorFactory = new BotConnectorFactory();
