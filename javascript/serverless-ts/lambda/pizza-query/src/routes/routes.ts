import Query from "../services/query";

class Routes {
	query: Query;

	constructor(query: Query) {
		this.query = query;
	}

	buildDefault() {
		return [
			{
				method: 'GET',
				path: '/pizzemQueryPizza',
				handler: request => this.query.run(request)
			}
		];
	}
}

export default Routes;
