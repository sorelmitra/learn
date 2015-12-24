var express = require('./config/express'); // Placing a "./" before the thing to 'require' causes the require API to look for it in our source code rather than in node_modules

var mongoose = require('./config/mongoose');

// The config/express.js file returns a function that is the actual Express function, thus it can be used transparently in place of the Express module

var port = process.env.PORT || 3000;

var db = mongoose();
db.on('error', console.error.bind(console, 'database connection error:'));

db.on('open', function() {
    var app = express();
    app.listen(port);
    module.exports = app;

    console.log("Listening on port " + port);
});

