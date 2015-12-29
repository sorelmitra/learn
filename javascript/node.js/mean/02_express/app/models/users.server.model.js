var mongoose = require('mongoose');

var UserSchema = new mongoose.Schema({
    firstName: String,
    lastName: String,
    email: String,
    username: String,
    password: String
});

mongoose.model('User', UserSchema);
