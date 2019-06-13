from channels.generic.websocket import JsonWebsocketConsumer

class NotificationsConsumer(JsonWebsocketConsumer):
	clients = []
	def connect(self):
		# Called on connection.
		# To accept the connection
		self.accept()
		c = self.scope["client"]
		self.clients.append(c)
		print(f'Chat API Socket opened {c}')

	def receive_json(self, content):
		# Called with json-decoded content when a message is received
		print(f'< {content}')
		self.send_json({'server': 'botagg'})

	def send_json(self, content):
		super().send_json(content)
		print(f'> {content}')

	def disconnect(self, close_code):
		# Called when the socket closes
		print(f'Chat API Socket closed with code {close_code}')

