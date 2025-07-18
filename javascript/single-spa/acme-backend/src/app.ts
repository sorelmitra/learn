import express from 'express';

import { getAllHouses, getHouseById } from './services/house.service';
import { asyncErrorHandler, errorHandlingMiddleware } from './exceptions';

const app = express();
const port = 9500;

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
