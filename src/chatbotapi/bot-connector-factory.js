var logService = require('./../chatmob/utils/log-service');

class BotConnectorFactory {

	get(botConnectorInstance) {
		let botConnectorType = process.env[`BOT_CONNECTOR_${botConnectorInstance}_TYPE`];
		let botConnectorUri = process.env[`BOT_CONNECTOR_${botConnectorInstance}_URI`];
		return this.getByTypeAndUri(botConnectorType, botConnectorUri);
	}

	getByTypeAndUri(botConnectorType, botConnectorUri) {
		if (botConnectorType == "internal") {
			return this.getInternalBotConnector(botConnectorUri);
		}
		throw `Unknown bot type ${botConnectorType}`; 
	}

	getInternalBotConnector(botConnectorUri) {
		let uri = this.getUri(botConnectorUri);
		if (uri.protocol != 'file') {
			throw `Unsupported protocol ${uri.protocol} for internal bot`;
		}
		logService.debug(this, `Creating internal bot ${uri.path}`);
		let BotClass = require(uri.path).default;
		let bot = new BotClass();
		return bot;
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
