import express from "express";
import oidc from "express-openid-connect";
import * as dotenv from "dotenv";

import {LOG} from "./log.js";

dotenv.config({path: '.trial.env'});

console.log(process.env);

const {auth, requiresAuth} = oidc;

const app = express();

const auth0_config = {
  authRequired: false,
  auth0Logout: true,
  secret: process.env.GENERATED_SECRET,
  baseURL: process.env.BASE_URL,
  clientID: process.env.AUTH0_CLIENT_ID,
  issuerBaseURL: process.env.ISSUER_BASE_URL
};

// auth router attaches /login, /logout, and /callback routes to the baseURL
app.use(auth(auth0_config));

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