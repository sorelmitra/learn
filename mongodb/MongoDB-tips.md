# Query

Find all failed accounting‑sync records for that organization, created after the given date, whose error message does not mention a QueryParserError:

```js
db.yourCollection.find({
  "organizationId": "e617aded-b4cd-4257-adce-a736023b4e30",
  "accountingSyncStatus": "Failed",
  "createDate": {
    "$gt": new Date("2025-07-19")
  },
  "accountingSyncErrorSummary": {
    "$not": /QueryParserError/
  }
});
```


# Dump & Restore

## Dump from a cloud server

Create `$HOME/.mongodb-cloud-config.yml` as this:

```yml
uri: "mongodb+srv://payment-dev@payment-dev.3jseb.mongodb.net/goodleap-accounting"
password: "UrDTcPZZY8wmO6XH"
```

Set strict permissions on the config file:

```shell
chmod 600 $HOME/.mongodb-cloud-config.yml
```

Dump:

```shell
mongodump \
	--config=$HOME/.mongodb-cloud-config.yml \
	--gzip \
	--db=goodleap-accounting \
	--archive=$HOME/Downloads/goodleap-accounting.archive.gz
```

## Restore to a local server

Create `$HOME/.mongodb-local-config.yml` as this:

```yml
uri: "mongodb://root@localhost:27017"
password: "pass123"
```

Set strict permissions on the config file:

```shell
chmod 600 $HOME/.mongodb-local-config.yml
```

Restore:

```shell
mongorestore \
	--config=$HOME/.mongodb-local-config.yml \
	--drop \
	--gzip \
	--nsInclude="goodleap-accounting.*" \
	--archive=$HOME/Downloads/goodleap-accounting.archive.gz
```
