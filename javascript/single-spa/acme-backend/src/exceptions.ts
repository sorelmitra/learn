export abstract class RequestException extends Error {
	status: number;
}

export class NotFoundException extends RequestException {
	constructor(message: string) {
		super(message);
		this.status = 404;
	}
}

/**
 * Wrap your handlers with this function to obtain
 * automatic error handling.
 */
export const asyncErrorHandler = fn => async (req, res, next) => {
  try {
    await fn(req, res);
  } catch (err) {
    next(err);
  }
};

/**
 * Use this middleware as your last Express call before `listen`
 * in order to handle your exceptions gracefully.
 */
export const errorHandlingMiddleware = (err, req, res, next) => {
  if (err instanceof RequestException) {
    const requestException = err as RequestException;
    res.status(requestException.status).send({ error: requestException.message });
    return;
  }
  next(err);
};

