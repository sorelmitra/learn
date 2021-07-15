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
					try {
						return await this.query.run(request);
					} catch (error) {
						return {
							oooooops: error
						}
					}
				}
			}
		];
	}
}

export default Routes;
