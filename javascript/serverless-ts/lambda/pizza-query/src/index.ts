import { transformRequest, transformResponse } from 'hapi-lambda';

import api from './api';

let server;
export const handler = async (event, context) => {
  // see https://docs.atlas.mongodb.com/best-practices-connecting-to-aws-lambda/
  context.callbackWaitsForEmptyEventLoop = false;

  console.log("INIT");
  if (!server) server = await api.init();
  console.log("transformRequest", event);
  const req = transformRequest(event);

  console.log("inject", req);
  const res = await server.inject(req);
  console.log("transformResponse", res);
  const response = transformResponse(res);

  return response;
};
