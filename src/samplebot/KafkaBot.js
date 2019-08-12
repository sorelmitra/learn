const Kafka = require('node-rdkafka');

exports.KafkaBot = function KafkaBot() {
	this.run = function run() {
		this.consumer = Kafka.KafkaConsumer({
			'group.id': `${process.env.KAFKA_GROUP_ID}`,
			'metadata.broker.list': `${process.env.KAFKA_HOST}:${process.env.KAFKA_PORT}`
		});

		this.consumer.connect();
		this.process();
	}

	this.process = function process() {
		this.consumer
		.on('ready', function() {
			this.consumer.subscribe([`${process.env.KAFKA_TOPIC}`]);
			this.consumer.consume();
		})
		.on('data', function(data) {
			console.log(data.value.toString());
		});
	}
	return this;
}
