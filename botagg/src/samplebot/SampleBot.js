const Botml = require('botml/lib/botml')

const SampleBotEvents = {
	REPLY: 'reply'
}

const BotmlEvents = {
	REPLY: 'reply'
}

class SampleBot {

	constructor() {
		this.bot = new Botml('./sample.bot');
		this.bot.start();
		this.cbs = [];
		let self = this;
		this.bot.on(BotmlEvents.REPLY, function(botResponse) {
			let response = {
				success: true,
				botResponse: {
					body: `${botResponse}`
				},
				reason: "chat bot response"
			};
			self.cbs[SampleBotEvents.REPLY](response);
		});
	}

	process(input) {
		this.bot.send(input.body);
	}

	on(event, cb) {
		this.cbs[event] = cb;
	}
}

exports.bot = SampleBot;
exports.events = SampleBotEvents;
