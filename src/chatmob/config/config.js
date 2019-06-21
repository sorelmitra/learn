
console.log("Environment: ", process.env.REACT_APP_BOTAGG_MOB_ENV);
var configName = 'config-' + process.env.REACT_APP_BOTAGG_MOB_ENV;
exports.config = require(`./${configName}.json`);
