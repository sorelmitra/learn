from channels.generic.websocket import JsonWebsocketConsumer

from ..lib.postnotif import *

class NotificationsConsumer(JsonWebsocketConsumer):
	def __init__(self, *args, **kwargs):
		super().__init__(*args, **kwargs)
		self.client = None
		self.register = getPostNotificationsRegister()

	def connect(self):
		# Called on connection.
		# To accept the connection
		self.accept()
		self.client = self.scope["client"]
		self.register.prepareClientForRegistration(self)
		print(f'Client {self.client} preparing for registration')

	def receive_json(self, content):
		# Called with json-decoded content when a message is received
		print(f'< {content}')
		result = self.register.registerClient(self, content)
		print(f'Client {self.client} registration status: {result["success"]}')
		self.send_json(result)

	def send_json(self, content):
		super().send_json(content)
		print(f'> {content}')

	def disconnect(self, close_code):
		# Called when the socket closes
		self.register.deregisterClient(self)
		print(f'Client {self.client} closed with code {close_code}')

