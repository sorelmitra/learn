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
				handler: async (request) => await this.repo.getAllPizzas()
			},
			{
				method: 'GET',
				path: '/pizzas/:{pizzaType}',
				handler: async (request) => await this.repo.getPizzaByType(request.params.pizzaType)
			},
			{
				method: 'POST',
				path: '/pizzas',
				handler: async (request) => await this.repo.createPizza(request.payload)
			},
			{
				method: 'POST',
				path: '/orders',
				handler: async (request) => await this.repo.createOrder(request.payload)
			},
			{
				method: 'DELETE',
				path: '/orders/:{id}',
				handler: async (request) => await this.repo.deleteOrder(request.params.id)
			},
			{
				method: 'patch',
				path: '/orders/:{id}',
				handler: async (request) => await this.repo.patchOrder(request.params.id, request.payload)
			},
		];
	}
}

export default Routes;
