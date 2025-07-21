import express from 'express';
import cors from 'cors';

import { getAllHouses, getHouseById } from './services/house.service';
import { asyncErrorHandler, errorHandlingMiddleware } from './exceptions';

const app = express();
const port = 9500;

app.use(cors({
  origin: 'http://localhost:9000', // allow only this origin
  methods: ['GET','POST','PUT','DELETE'], // allowed HTTP methods
  allowedHeaders: ['*'], // allowed headers
  credentials: true, // if you need cookies/auth
}));

app.get('/health', (req, res) => {
  res.send('Hello World!');
});

app.get('/houses', asyncErrorHandler(async (req, res) => {
  res.send(await getAllHouses());
}));

app.get('/houses/:id', asyncErrorHandler(async (req, res) => {
  res.send(await getHouseById(req.params.id));
}));

// This MUST be the last one before calling `listen`
app.use(errorHandlingMiddleware);
app.listen(port, () => {
  return console.log(`Express is listening at http://localhost:${port}`);
});
