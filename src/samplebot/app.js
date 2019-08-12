require('dotenv').config()

var KafkaBot = require('./KafkaBot').KafkaBot;

var bot = new KafkaBot();
bot.run()
