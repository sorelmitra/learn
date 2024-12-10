import queue


class EndpointResult:
	"""
	Represents the result of an endpoint call.

	Attributes:
		status_code (int): The HTTP status code of the endpoint response. Defaults to 200.
		response_data (any): The data returned by the endpoint. Default is None.
	"""

	def __init__(self, *, status_code=200, response_data=None):
		"""
		Initializes a new instance of EndpointResult.

		Parameters:
			status_code (int): The HTTP status code of the response. Defaults to 200.
			response_data (any): The data returned by the endpoint. Defaults to None.
		"""
		self.status_code = status_code
		self.response_data = response_data

	def is_successful(self):
		"""
		Determines if the endpoint call was successful.

		Returns:
			bool: True if the status code is within the 200-299 range, indicating success.
		"""
		return 200 <= self.status_code <= 299


class EndpointResultConfig:
	"""
	Holds configuration and state for managing a sequence of EndpointResult instances.

	Attributes:
		response_queue (queue.Queue): Queue object for storing response data.
		available_results (list[EndpointResult]): Pre-configured results for simulation.
		index (int): Current position within available_results.
		did_previous_call_fail (bool): Indicates if the previous call was a failure.
		success_status_code (int): Status code considered as a successful call. Defaults to 200.
		status_code_after_fail (int): Status code to use after a previous call fails. Defaults to 202.
	"""

	def __init__(self, *, response_queue: queue.Queue, available_results: list[EndpointResult],
				 success_status_code=200, status_code_after_fail=202):
		"""
		Initializes a new instance of EndpointResultConfig.

		Parameters:
			response_queue (queue.Queue): Queue object for storing response data.
			available_results (list[EndpointResult]): Pre-configured results for simulation. If None, an empty list is used.
			success_status_code (int): Status code considered as a successful call. Defaults to 200.
			status_code_after_fail (int): Status code to use after a previous call fails. Defaults to 202.
		"""
		self.response_queue = response_queue
		self.available_results = available_results if available_results is not None else []
		self.index = 0
		self.did_previous_call_fail = False
		self.success_status_code = success_status_code
		self.status_code_after_fail = status_code_after_fail


class EndpointResultComputer:
	"""
	Computes and retrieves EndpointResult instances based on configuration.

	Attributes:
		store (dict): Dictionary storing various objects including EndpointResultConfig.
	"""

	def __init__(self, _store: dict):
		"""
		Initializes a new instance of EndpointResultComputer.

		Parameters:
			_store (dict): Dictionary for storing configuration and state.
		"""
		self.store = _store

	def next(self, current_payload: dict=None):
		"""
		Computes the next EndpointResult based on current configuration and payload.

		Parameters:
			current_payload (dict): The payload for the current call. Used if no pre-configured result is available.

		Returns:
			EndpointResult: The next result based on the current state and given payload.
		"""
		config: EndpointResultConfig = self.store.get(EndpointResultConfig)

		# Use the next available result or create a new one with the current payload
		if config.index < len(config.available_results):
			endpoint_result = config.available_results[config.index]
			# Set response data to indicate success if not already set
			if endpoint_result.response_data is None:
				endpoint_result.response_data = {"success": endpoint_result.is_successful()}
		else:
			endpoint_result = EndpointResult(
				status_code=200,
				response_data=current_payload if current_payload is not None else {
					"success": True}
			)

		# Update the status code after a failure if the current call is successful
		if config.did_previous_call_fail and endpoint_result.is_successful():
			endpoint_result.status_code = config.status_code_after_fail

		# Track failure for the next call
		if not endpoint_result.is_successful():
			config.did_previous_call_fail = True

		# Move to the next result
		config.index += 1

		# Update the store with the modified configuration
		self.store[EndpointResultConfig] = config
		return endpoint_result
