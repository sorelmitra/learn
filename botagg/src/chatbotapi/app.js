require('dotenv').config()

var ChatBotApi = require('./ChatBotApi').ChatBotApi;

var api = new ChatBotApi();
api.run()
