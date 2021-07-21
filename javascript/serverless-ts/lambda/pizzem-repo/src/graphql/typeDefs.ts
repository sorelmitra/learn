import {gql} from "apollo-server-lambda";

export const typeDefs = gql`
    type Query {
        hello: String
        pizzas(type: String): [Pizza]!
    }
    input PizzaInput {
        type: String!
        description: String!
    }
    type Mutation {
        createPizza(pizza: PizzaInput!): Pizza!
    }
    type Pizza {
        type: String
        description: String
        error: String
    }
`;

