var http = require('http');

var port = 3001;

http.createServer(function(req, res) {
	res.writeHead(200, {
		'Content-Type': 'text/plain'
	});
	res.end('This is a web server created with the HTTP module.');
})
.listen(port);

console.log('Listening on port ' + port);
