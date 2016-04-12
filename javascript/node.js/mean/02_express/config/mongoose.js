var config = require('./config'),
    mongoose = require('mongoose');
    
module.exports = function() {
    // Connect to the configured database
    mongoose.connect(config.db);
    // Load the User data model schema
    require('../app/models/users.server.model.js');
    // Return the "connection" function of the mongoose object
    return mongoose.connection;
};
