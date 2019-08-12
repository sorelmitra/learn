var logService = require('./../chatmob/utils/log-service');
const Kafka = require('node-rdkafka');

class KafkaBot {
	run() {
		this.consumer = Kafka.KafkaConsumer({
			'group.id': `${process.env.KAFKA_GROUP_ID}`,
			'metadata.broker.list': `${process.env.KAFKA_HOST}:${process.env.KAFKA_PORT}`
		});

		logService.debug(this, `Connecting...`);
		let resp = this.consumer.connect();
		logService.debug(this, `Connection status:`, resp);
		this.process();
	}

	process() {
		let self = this;
		this.consumer
		.on('ready', function() {
			logService.debug(self, `Kafka connection ready`);
			self.consumer.subscribe([`${process.env.KAFKA_TOPIC}`]);
			logService.debug(self, `Subscribed to topic, consuming...`);
			self.consumer.consume();
		})
		.on('data', function(data) {
			logService.debug(self, data.value.toString());
		});
	}
}

exports.KafkaBot = KafkaBot;
