import Repo from "../services/repo";

export class Resolvers {
    repo: Repo;

    constructor(repo: Repo) {
        this.repo = repo;
    }

    buildDefault() {
        return {
            Query: {
                hello: () => 'Hello world!',
                pizzas: async (_obj, args, _context, _info) => {
                    if (args.type) {
                        return await this.repo.getPizzaByType(args.type);
                    }
                    return await this.repo.getAllPizzas();
                },
            },
        };
    }
}

