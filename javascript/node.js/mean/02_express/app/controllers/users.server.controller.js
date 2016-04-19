// User is a sort of a class. It is returned by the mongoose library, via its model() function, that gets the data model name. In this case, the data model name is User, which we created in users.server.model.js and registered with mongoose in config/mongoose.js
var User = require('mongoose').model('User');

exports.create = function(req, res, next) {
    var user = new User(req.body);
    user.save(function(err) {
        if (err) {
            return next(err);
        }
        res.json(user);
    })
};

exports.get = function(req, res, next) {
    res.json(200, {
        message: "Will be implemented soon"
    });
};

