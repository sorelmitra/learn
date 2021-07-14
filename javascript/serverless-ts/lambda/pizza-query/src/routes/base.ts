import { queryService } from "../services/query";

export default [
	{
		method: 'POST',
		path: '/pizzemQueryPizza',
		handler: request => queryService.run(request)
	}
];
