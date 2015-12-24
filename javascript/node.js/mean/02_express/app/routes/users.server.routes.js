module.exports = function(app) {
    var users = require('../controllers/users.server.controller');
    app.post('/users', users.create);
};
