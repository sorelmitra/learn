require('dotenv').config()

var SampleBotServer = require('./SampleBotServer').SampleBotServer;

var bot = new SampleBotServer();
bot.run()
