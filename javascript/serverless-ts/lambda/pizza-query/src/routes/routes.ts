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
				handler: async (request) => {
					let r = await this.query.run(request);
					return r;
				}
			}
		];
	}
}

export default Routes;
