const express = require('express');

const app = express();

app.use(express.static('src'));

app.get('/', (req, res) => {
  res.send('Successful response.');
});

app.listen(3203, () => console.log('Local Server is listening on port 3203.'));
