var config = require('./config'),
    express = require('express'),
	morgan = require('morgan'),
	compress = require('compression'),
	bodyParser = require('body-parser'),
	methodOverride = require('method-override'),
    session = require('express-session');

module.exports = function() {
	var app = express();
	
	if (process.env.NODE_ENV == 'development') {
		app.use(morgan('dev')); // During development use the morgan logger
	} else if (process.env.NODE_ENV == 'production') {
		app.use(compress()); // In production, compress all responses
	} else {
		throw 'Unknown NODE_ENV value <' + process.env.NODE_ENV + '>';
	}
	
	app.use(bodyParser.urlencoded({
		extended: true
	}));
	app.use(bodyParser());
	app.use(methodOverride());
	
	app.set('views', './app/views'); // The views will be searched in this directory
	app.set('view engine', 'ejs'); // The view engine is EJS
    
    app.use(express.static('./public')); // Serve static files from the given directory
    
    // The express-session middleware adds sessions by using cookies
    app.use(session({
        secret: config.sessionSecret, // The session secret, taken from config
        saveUninitialized: true, // TODO what does this mean?
        resave: true // TODO what does this mean?
    }));
	
	var route = require('../app/routes/index.server.routes'); // get our index routes
	route(app); // this connects the route to the controller
	
	return app;
};
