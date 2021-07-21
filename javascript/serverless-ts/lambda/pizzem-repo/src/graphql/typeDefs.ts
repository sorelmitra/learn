import {gql} from "apollo-server-lambda";

export const typeDefs = gql`
    type Query {
        hello: String
        pizzas(type: String): [Pizza]!
    }
    type Pizza {
        type: String
        description: String
    }
`;

