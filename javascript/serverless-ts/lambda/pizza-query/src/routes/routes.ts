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
				path: '/pizzas',
				handler: async (request) => await this.query.getAll()
			},
			{
				method: 'GET',
				path: '/pizzas/:{pizzaType}',
				handler: async (request) => await this.query.getByType(request.params.pizzaType)
			}
		];
	}
}

export default Routes;
