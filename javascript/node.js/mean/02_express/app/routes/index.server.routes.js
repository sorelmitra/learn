// The Express module knows how to load and use this file based on the config defined in app/config/express.js

module.exports = function(app) {
	var index = require('../controllers/index.server.controller');
	app.get('/', index.render); // handle the '/' route by using the render() function that was exported in the index.server.controller.js 
}
