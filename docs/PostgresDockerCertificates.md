To get Postgres in Docker to login users using certificates, you need to follow these steps:

# 1 Get a Certificate Authority to Sign your Certificates

It can be your own CA for development or an official one for production.

For your own CA, read this: https://deliciousbrains.com/ssl-certificate-authority-for-local-https-development/

# 2 Create the Directory Structure

Before you start: Make sure the certificates and keys you're about to create are NOT added to Source Control. They must be kept under private access for restricted people.

Create a "postgres" directory with this structure:

- postgres
	- ssl
		- client

# 3 Create Server Certificates

CD to "postgres/ssl" directory.

Save this file as "gen-certificates.sh":

	BASEDIR=$1
	shift

	if [[ ! -d "${BASEDIR}" ]]; then
		BASEDIR=.
	fi

	echo "Generating Certificates in directory ${BASEDIR}"
	echo

	LOGGING_PREFIX="$(filename "$0") >> "

	ROOT_CA_DIR="~/CA"
	ROOT_CA_CERTIFICATE="${ROOT_CA_DIR}/CARoot.pem"
	ROOT_CA_KEY="${ROOT_CA_DIR}/CARoot.key"

	CERTNAME="server"
	COUNTRY_CODE="RO"
	STATE="some state"
	LOCALITY="some city"
	ORGANIZATION="some organization"
	ORGANIZATIONAL_UNIT="some unit"
	COMMON_NAME="postgres"

	PASSKEY=password

	# generate a key for our certificate
	echo 
	echo "${LOGGING_PREFIX} Generating key for ${CERTNAME} certificate"
	openssl genrsa -des3 -passout pass:${PASSKEY} -out ${BASEDIR}/${CERTNAME}.pass.key 2048
	openssl rsa -passin pass:${PASSKEY} -in ${BASEDIR}/${CERTNAME}.pass.key -out ${BASEDIR}/${CERTNAME}.key
	rm ${BASEDIR}/${CERTNAME}.pass.key
	echo

	# create a certificate request. This includes a subject alternative name so either aios-localhost, localhost or postgres_ssl can be used to address it
	echo
	echo "${LOGGING_PREFIX} Creating ${CERTNAME} certificate"
	openssl req -new -key ${BASEDIR}/${CERTNAME}.key -out ${BASEDIR}/${CERTNAME}.csr -subj "/C=${COUNTRY_CODE}/ST=${STATE}/L=${LOCALITY}/O=${ORGANIZATION}/OU=${ORGANIZATIONAL_UNIT}/CN=${COMMON_NAME}" -reqexts SAN -config <(cat /etc/ssl/openssl.cnf <(printf "[SAN]\nsubjectAltName=DNS:${COMMON_NAME},DNS:localhost")) 
	echo "${LOGGING_PREFIX} ${CERTNAME} certificate signing request (${BASEDIR}/${CERTNAME}.csr) is:"
	openssl req -verify -in ${BASEDIR}/${CERTNAME}.csr -text -noout
	echo

	# use our CA certificate and key to create a signed version of the ${CERTNAME} certificate
	echo 
	echo "${LOGGING_PREFIX} Signing ${CERTNAME} certificate using our root CA certificate and key"
	openssl x509 -req -sha256 -days 365 -in ${BASEDIR}/${CERTNAME}.csr -CA ${ROOT_CA_CERTIFICATE} -CAkey ${ROOT_CA_KEY} -CAcreateserial -out ${BASEDIR}/${CERTNAME}.crt -extensions SAN -extfile <(cat /etc/ssl/openssl.cnf <(printf "[SAN]\nsubjectAltName=DNS:${COMMON_NAME},DNS:localhost")) 
	chmod og-rwx ${BASEDIR}/${CERTNAME}.key
	echo "${LOGGING_PREFIX} ${CERTNAME} certificate signed with our root CA certificate (${BASEDIR}/${CERTNAME}.crt) is:"
	openssl x509 -in ${BASEDIR}/${CERTNAME}.crt -text -noout
	echo

	# done output the base64 encoded version of the root CA certificate which should be added to trust stores
	echo
	echo "${LOGGING_PREFIX} Done. Next time the postgres_ssl docker image is rebuilt the new ${CERTNAME} certificate (${BASEDIR}/${CERTNAME}.crt) will be used."
	echo
	echo "${LOGGING_PREFIX} Use the following CA certificate variables:"
	B64_CA_CERT=`cat ${ROOT_CA_CERTIFICATE} | base64`
	echo "POSTGRES_SSL_CA_CERT=${B64_CA_CERT}"


Edit "gen-certificates.sh"  and set:

	CERTNAME="server"

Run gen-certificates.sh. It will generate your server certificates.

# 4 Create Client Certificates

CD to "postgres/ssl/client" directory.

Edit "gen-certificates.sh" and set:

	CERTNAME="postgresql"

Run gen-certificates.sh. It will generate your client certificates.

# 5 Copy CA Root Certificate

Copy the CA Root certificate "myCA.pem" to "ssl" directory.

# 6 Create Postgres Configuration Files

CD to "postgres" directory.

Create "postgresql.conf":

	listen_addresses = '*'
	log_connections = on
	log_disconnections = on
	log_line_prefix = '%t %c %q%u@%h:%d '
	ssl = on 
	ssl_ca_file = '/var/lib/postgresql/root.crt'
	ssl_cert_file = '/var/lib/postgresql/server.crt' 
	ssl_key_file = '/var/lib/postgresql/server.key'

This will configure Postgres to read the keys from the specified paths that are internal to Docker.

Create "pg_hba.conf":

	hostssl  all  all  0.0.0.0/0  cert  clientcert=1
	hostnossl  all  all  0.0.0.0/0  reject

# 7 Create and Build a Docker Postgres Image

CD to "postgres" directory.

Create a Dockerfile for postgres based on an official one:

Dockerfile:

	FROM postgres:latest
	LABEL "Product"="PostgreSQL (SSL Certificate Enabled)"
	COPY ssl/server.key /var/lib/postgresql/server.key
	COPY ssl/server.crt /var/lib/postgresql/server.crt
	COPY ssl/myCA.pem /var/lib/postgresql/root.crt
	RUN chown postgres /var/lib/postgresql/server.key && \
	    chmod 600 /var/lib/postgresql/server.key

This will copy the server certificate and key and CA root certificate into the Postgres Docker image.

Build that Docker image:

	docker build -t postgres-cert:latest .

# 8 Create a Docker Volume

Create a Docker volume to hold your Postgres DB:

	docker volume create postgres10-data

# 9 Create a Docker-Compose File to Launch Postgres

CD to "postgres".

Create "docker-compose.yml":

	version: '3'

	networks:
	  botagg:
	    driver: bridge
	    ipam:
	      driver: default
	      config:
	        - subnet: 10.2.1.0/24

	volumes:
	  postgres10-data:
	    external: true

	services:
	  postgresql:
	    image: postgres-cert:latest
	    command: postgres -c 'config_file=/etc/postgresql/postgresql.conf' -c 'hba_file=/etc/postgresql/pg_hba.conf'
	    networks:
	      botagg:
	        ipv4_address: 10.2.1.11
	    ports:
	      - "5203:5432"
	    volumes:
	      - postgres10-data:/var/lib/postgresql/data
	      - ./postgresql.conf:/etc/postgresql/postgresql.conf
	      - ./pg_hba.conf:/etc/postgresql/pg_hba.conf
	    environment:
	      - POSTGRES_PASSWORD=postgres

Notice the use of "./" in the volumes - "." specifies to docker-compose we have a path, and that path is relative to the docker-compose.yml file, which is very important for running the docker-compose command from anywhere within the directory tree that has the yml file at the root.

# 10 (Re)Start Postgres

CD to where you've placed "docker-compose.yml" and run:

	docker-compose stop && docker-compose rm -f
	docker-compose up -d

This will stop, remove, and then start your Postgres Docker container.

# 11 Copy Client Certificates

For psql:

CD to "postgres/ssl/client" directory and run:

	cp postgresql.crt postgresql.key ~/.postgresql/
	cp ../myCA.pem ~/.postgresql/

This will copy the client certificate and key and the CA root certificate for the psql client to use.

For another client refer to that client's documentation for setting up certificates.

# 12 Connect to Postgres using Certificates

For psql:

Run this command:

	psql "sslmode=verify-ca host=192.168.99.100 port=5203" -U postgres

Replace the IP and port with whatever you have in your Docker setup.

For another client refer to that client's documentation for connecting.

# Troubleshooting

If you get an error like that:

	psql: FATAL:  certificate authentication failed for user "postgres"

Look in the Postgres server logs, it will show you the reason.
In my case, after doing all the above, the problem was that I did not create correct Common Names in the certificate.
For Postgres certificate validation to work, you have to have the Common Name == <User Name you use in the client>, for both the server and the client certificates.

Make sure you regenerate the Docker image each time you change the certificates and that you "stop", "rm" and "up -d" again the container in docker-compose!

# References

- Postgres Config file locations: https://www.postgresql.org/docs/current/runtime-config-file-locations.html
- Enabling SSL in Postgres: https://www.postgresql.org/docs/current/ssl-tcp.html
- Enable Certificate Login in Postgres: https://www.postgresql.org/docs/current/auth-pg-hba-conf.html
- Enabling Certificate Validation in the Client: https://www.postgresql.org/docs/current/libpq-ssl.html
- The whole process explained: https://www.depesz.com/2015/05/11/how-to-setup-ssl-connections-and-authentication/
