var config = require('./config'),
    mongoose = require('mongoose');
    
module.exports = function() {
    mongoose.connect(config.db);
    return mongoose.connection;
};
