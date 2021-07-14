import { queryService } from "../services/query";

export default [
	{
		method: 'GET',
		path: '/pizzemQueryPizza',
		handler: request => queryService.run(request)
	}
];
