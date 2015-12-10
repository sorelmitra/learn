var connect = require('connect');

var app = connect();
var port = 3002;

var logger = function (req, res, next) {
	console.log(req.method, req.url);
	next();
};

var hi = function(req, res, next) {
	res.setHeader('Content-Type', 'text/plain');
	res.end('Hi there from Connect middleware.');
};

var bye = function(req, res, next) {
	res.setHeader('Content-Type', 'text/plain');
	res.end('Bye from Connect middleware.');
};

app.use(logger);
app.use('/hi', hi);
app.use('/bye', bye);

app.listen(port);
console.log('Listening on port ' + port);
