from .Commands import *

class PostNotifCommandsFactory:
	def __init__(self, register, consumer):
		self.register = register
		self.consumer = consumer

	def create(self, content):
		commandString = content['command']
		if commandString == "register":
			return CommandRegister(self.register, self.consumer, content)
		else:
			return CommandNotUnderstood(self.register, self.consumer, content)
