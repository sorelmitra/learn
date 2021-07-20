import Repo from "../src/services/repo";
import mock = jest.mock;
import {Db, DbError, DbModel, DbOptions} from "../src/db/db";

let expectedPizza: Pizza = {
    type: "unittest1",
    description: "A unit test pizza",
};

let mockDb = {
    create: jest.fn((options: DbOptions): Promise<DbModel> => {
        return Promise.resolve(expectedPizza);
    }),
    delete: jest.fn((options: DbOptions): Promise<DbModel> => {
        return Promise.resolve(expectedPizza);
    }),
    getAll: jest.fn((options: DbOptions): Promise<DbModel[]> => {
        return Promise.resolve([expectedPizza]);
    }),
    patch: jest.fn((options: DbOptions): Promise<DbModel> => {
        return Promise.resolve(expectedPizza);
    })
};
let repo: Repo = new Repo(mockDb);

describe("Repo service tests", () => {
    it('should fail to get pizzas if table name is missing', async function () {
        let error: DbError[] = [{error: "Missing environment variable PIZZA_TYPES_TABLE!"}];
        let actualPizza = await repo.getAllPizzas();
        expect(actualPizza).toMatchObject(error);
        expect(mockDb.getAll.mock.calls.length).toBe(0);
    });

    it('should get all pizzas from DB', async function () {
        process.env.PIZZA_TYPES_TABLE = "dummy";
        let actualPizza = await repo.getAllPizzas();
        expect(actualPizza).toMatchObject([expectedPizza]);
        expect(mockDb.getAll.mock.calls.length).toBe(1);
    });
});
