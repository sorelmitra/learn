# Consume or Produce Locally

Sources:

[1] https://github.com/edenhill/kcat
[2] https://github.com/edenhill/kcat/issues/247

## With API keys and certificates

Configure server and keys:

```shell
KAFKA_BOOTSTRAP_SERVER=xxx.us-west-2.aws.confluent.cloud:9092 && KAFKA_API_KEY='...' && KAFKA_API_SECRET='...'
```

Download your certificate to `.../ca.cert.pem`.

Consume:

```shell
kcat -m 60 -b ${KAFKA_BOOTSTRAP_SERVER} -X security.protocol=SASL_SSL -X sasl.mechanisms=PLAIN -X sasl.username=${KAFKA_API_KEY} -X sasl.password=${KAFKA_API_SECRET} -X ssl.ca.location=.../ca.cert.pem -X api.version.request=true -t my.topic
```

This will read messages from the specified topic and output their content to the console.  Headers are not displayed.

## With username and password

Configure server and password:

```shell
KAFKA_BOOTSTRAP_SERVER=xxx.us-west-2.aws.confluent.cloud:9092 && KAFKA_USERNAME='...' && KAFKA_API_SECRET='...'
```

Consume:

```shell
kcat -m 60 -b ${KAFKA_BOOTSTRAP_SERVER} -X security.protocol=SASL_SSL -X sasl.mechanisms=PLAIN -X sasl.username=${KAFKA_USERNAME} -X sasl.password=${KAFKA_PASSWORD} -X api.version.request=true -t my.topic
```
