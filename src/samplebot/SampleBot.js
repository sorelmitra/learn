class SampleBot {

	constructor() {
	}

	process(input) {
		let response = {
			success: true,
			botResponse: {
				body: `SampleBot is on the way: '${input.body}'`
			},
			reason: "chat bot response"
		};
		return response;
	}
}

exports.default = SampleBot;
