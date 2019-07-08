var logService = require('../chatmob/utils/log-service');
var WebSocketServer = require('websocket').server;
var http = require('http');
var SampleBot = require('./SampleBot').default;
 
class SampleBotServer {

	constructor() {
		this.port = process.env.PORT;
		this.wsServer = null;
		this.connection = null;
		this.bot = new SampleBot();
	}

	run() {
		var server = http.createServer(function(request, response) {
			logService.debug(this, 'Received request for ' + request.url);
			response.writeHead(404);
			response.end();
		});
		let self = this;
		server.listen(this.port, function() {
			logService.debug(this, `Server is listening on port ${self.port}`);
		});
		this.wsServer = new WebSocketServer({
			httpServer: server,
			// You should not use autoAcceptConnections for production
			// applications, as it defeats all standard cross-origin protection
			// facilities built into the protocol and the browser.  You should
			// *always* verify the connection's origin and decide whether or not
			// to accept it.
			autoAcceptConnections: false
		});
		this.wsServer.on('request', function(request) {
			if (!self.originIsAllowed(request.origin)) {
			  // Make sure we only accept requests from an allowed origin
			  request.reject();
			  logService.debug(self, 'Connection from origin ' + request.origin + ' rejected.');
			  return;
			}
			
			try {
				self.connection = request.accept(null, request.origin);
				logService.debug(self, 'Connection accepted.');
				self.connection.on('message', function(message) {
					if (message.type === 'utf8') {
						self.processMessage(message.utf8Data);
					}
					else if (message.type === 'binary') {
						logService.debug(self, 'Received Binary Message of ' + message.binaryData.length + ' bytes');
						//self.connection.sendBytes(message.binaryData);
					}
				});
				self.connection.on('close', function(reasonCode, description) {
					logService.debug(self, 'Peer ' + self.connection.remoteAddress + ' disconnected.');
				});
			} catch (error) {
				logService.error(this, error);
			}
		});	
	}

	originIsAllowed(origin) {
		// TODO: put logic here to detect whether the specified origin is allowed.
		return true;
	}

	processMessage(utf8Data) {
		logService.debug(this, `< ${utf8Data}`);
		let o = JSON.parse(utf8Data);
		if (o.command == "register") {
			this.processRegistration(o);
		} else if (o.command == "message") {
			let response = this.bot.process(o);
			this.send(response);
		} else {
			logService.error(this, `Unknown command ${o.command}`);
		}
	}

	processRegistration(input) {
		let response = {
			success: true,
			"client-registration": {
				name: input.name
			},
			reason: "registration succeeded"
		};
		this.send(response);
	}

	send(response) {
		let s = JSON.stringify(response);
		logService.debug(this, `> ${s}`);
		this.connection.sendUTF(s);
	}
}

exports.SampleBotServer = SampleBotServer;
