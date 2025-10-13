# Connect Without Password Prompt

Create a `~/.pgpass` file with lines of the form: `hostname:port:database:username:password`.

Then connect with this command:

`/opt/homebrew/Cellar/libpq/17.5/bin/psql postgresql://username@hostname:port/database`

# Dump and Restore Database

Dump database:

```shell
timestamp=`date +"%Y-%m-%d-%H%M"` && db=payment_party && /opt/homebrew/Cellar/libpq/17.5/bin/pg_dump --verbose --host=localhost --port=5435 --username=postgres --format=c --encoding=Big5 --file /Users/sorel/Downloads/dump-${db}-${timestamp}.sql ${db}
```

Restore database:

```shell
db=payment_party && /opt/homebrew/Cellar/libpq/17.5/bin/pg_restore --verbose --host=localhost --port=5435 --username=postgres --clean --format=c --dbname=payment_party /Users/sorel/Downloads/dump-${db}-*
```
