class CommandBase:
	def __init__(self, register, consumer, content):
		self.register = register
		self.consumer = consumer
		self.content = content

	def execute(self):
		raise NotImplementedError("Please inherit from me!")

class CommandNotUnderstood(CommandBase):
	def execute(self):
		result = {
			'success': False,
			'client': {
				'name': self.content['name']
			},
			'reason': f'command not understood'
		}
		return result

class CommandRegister(CommandBase):
	def execute(self):
		result = self.register.registerClient(self.consumer, self.content)
		print(f'Client {self.consumer.client} registration status: {result["success"]}')
		return result

