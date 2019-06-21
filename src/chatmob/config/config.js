
console.log("Environment: ", process.env.REACT_APP_BOTAGG_MOB_ENV);
var configName = 'config-' + process.env.REACT_APP_BOTAGG_MOB_ENV;
config = require(`./${configName}.json`);
config['chat']['name'] = process.env.BOTAGG_CHAT_NAME;
console.log('Config: ', config);

exports.config = config;
