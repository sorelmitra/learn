var mongoose = require('mongoose');

// Declare the data model for a User
var UserSchema = new mongoose.Schema({
    firstName: String,
    lastName: String,
    email: String,
    username: String,
    password: String
});

// Define the actual Mongoose schema for the User data model
mongoose.model('User', UserSchema);
