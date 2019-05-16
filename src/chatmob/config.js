
console.log("Environment: ", process.env.REACT_APP_BOTAGG_MOB_ENV);
var configName = 'config-' + process.env.REACT_APP_BOTAGG_MOB_ENV;
export default config = require(`./${configName}.json`);
