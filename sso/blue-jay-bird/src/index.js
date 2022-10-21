import express from "express";

import oidc from "express-openid-connect";
import {LOG} from "./log.js";

const {auth, requiresAuth} = oidc;

const app = express();

const config = {
  authRequired: false,
  auth0Logout: true,
  secret: '4c8fe981070519ed4cbd6b4d80f373536ef1f253fc1da9625f2a91a3663baa5e',
  baseURL: 'http://localhost:8080',
  clientID: '<your-client-id>',
  issuerBaseURL: 'https://<your-auth0-tenant>.auth0.com'
};

// auth router attaches /login, /logout, and /callback routes to the baseURL
app.use(auth(config));

// req.isAuthenticated is provided from the auth router
app.get('/', (req, res) => {
  LOG.trace('Request', req);
  res.send(req.oidc.isAuthenticated() ? 'Logged in' : 'Logged out');
});

app.get('/profile', requiresAuth(), (req, res) => {
  res.send(JSON.stringify(req.oidc.user));
});

const server = app.listen(8080, 'localhost', function () {
  const host = server.address().address;
  const port = server.address().port;

  console.log("Example app listening at http://%s:%s", host, port)
});