// The Express module knows how to load and use this file based on the config defined in app/config/express.js

// export the render() function
exports.render = function(req, res) {
	res.send('Hello there');
}
