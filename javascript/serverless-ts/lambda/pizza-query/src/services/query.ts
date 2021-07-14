
class Query {
	constructor() {
	}

	run(request) {
		return [{ type: "carbonara", description: "Meat me!" }, { type: "quattro-stagioni", description: "Cheese me!" }];
	}
}

export default Query;
