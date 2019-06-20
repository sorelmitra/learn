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

class CommandNotifyAll(CommandBase):
	def execute(self):
		post = self.content['post']
		result = {
			'success': True,
			'post': post,
			'reason': f'message posted notification'
		}
		count = self.register.notifyAll(result)
		print(f'Notified {count} consumers on post {post}')
		return result
