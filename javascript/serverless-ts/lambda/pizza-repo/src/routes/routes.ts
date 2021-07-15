import Repo from "../services/repo";

class Routes {
	repo: Repo;

	constructor(repo: Repo) {
		this.repo = repo;
	}

	buildDefault() {
		return [
			{
				method: 'GET',
				path: '/pizzas',
				handler: async (request) => await this.repo.getAll()
			},
			{
				method: 'GET',
				path: '/pizzas/:{pizzaType}',
				handler: async (request) => await this.repo.getByType(request.params.pizzaType)
			}
		];
	}
}

export default Routes;
