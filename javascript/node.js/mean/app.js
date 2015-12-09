var express = require('express');

var app = express();

var port = process.env.PORT || 5000;

var router = express.Router();

app.use('/api', router);

app.get('/', function(req, res) {
	res.send('Hi there, I\'m learning MEAN web development.');
});

app.listen(port, function() {
	console.log('Listening on port: ' + port);
});
