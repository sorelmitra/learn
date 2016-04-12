var express = require('./config/express'); // Placing a "./" before the thing to 'require' causes the require API to look for it in our source code rather than in node_modules

// mongoose is a function because of the way config/mongoose.js is implemented
var mongoose = require('./config/mongoose');

// The config/express.js file returns a function that is the actual Express function, thus it can be used transparently in place of the Express module

var port = process.env.PORT || 3000;

// Calling the mongoose() function - which is the connection() function of the mongoose library - creates our database object. Now we can do error handling on it. The MEAN book suggests that the config/mongoose.js returns the result of mongoose.connect(), but I found this not to work (probably mongoose has changed since the book was written).
var db = mongoose();
db.on('error', console.error.bind(console, 'database connection error:'));

db.on('open', function() {
    var app = express();
    app.listen(port);
    module.exports = app;

    console.log("Listening on port " + port);
});

