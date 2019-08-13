import threading

#
# Singleton for use in NotificationsConsumer
# Since the NotificationsConsumer instances are created by
# Django Channels, one per client, I have to keep track of
# all those instances somewhere to be able to send to all
# when a message is posted.
# So I have this PostNotificationsRegister which is a
# singleton meant to act in a similar manner to Java Spring
# Boot Singletons.

register = None

def getPostNotificationsRegister():
	global register
	if (register is None):
		register = PostNotificationsRegister()
	return register

class PostNotificationsEntry:
	id = None
	registered = False

class PostNotificationsRegister:
	def __init__(self, *args, **kwargs):
		super().__init__(*args, **kwargs)
		self.consumers = {}
		self.id = 0

	def timeoutUnregisteredClient(self, consumer):
		if consumer not in self.consumers:
			return
		entry = self.consumers[consumer]
		if entry.registered:
			return
		print(f'Client {consumer.client} timed out')
		self.deregisterClient(consumer)

	def prepareClientForRegistration(self, consumer):
		self.id = self.id + 1
		entry = PostNotificationsEntry()
		entry.id = self.id
		self.consumers[consumer] = entry
		t = threading.Timer(2, self.timeoutUnregisteredClient, [consumer], {})
		t.start()

	def registerClient(self, consumer, input):
		clientName = None
		try:
			clientName = input['name']
		except KeyError:
			return {
				'success': False,
				'notification-registration': {},
				'reason': "Cannot register unnamed client. Please add a 'name' field"
			}

		if consumer in self.consumers:
			entry = self.consumers[consumer]
			entry.registered = True
			result = {
				'success': True,
				'notification-registration': {
					'id': entry.id,
					'name': clientName
				},
				'reason': 'registration succeeded'
			}
		else:
			result = {
				'success': False,
				'notification-registration': {
					'id': None,
					'name': clientName
				},
				'reason': f'registration failed: could not find entry for client {input["name"]}'
			}
		return result

	def deregisterClient(self, consumer):
		if consumer in self.consumers:
			del self.consumers[consumer]

	def notifyAll(self, content):
		for consumer in self.consumers:
			consumer.send_json(content)
		return len(self.consumers)

