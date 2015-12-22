// The Express module knows how to load and use this file based on the config defined in app/config/express.js

// export the render() function
exports.render = function(req, res) {
    if (req.session.lastVisit) {
        console.log('Last visit: ' + req.session.lastVisit);
    }
    req.session.lastVisit = new Date();

    // We render a view using the views location and the configured views engine
	res.render('index', { // the 'index' string maps to the index.<whatever> file saved in the views location indicated in config/express.js. <whatever> depends on the view engine
        title: 'Gee, man!'
    });
}
