# Jetty Secure WebSockets #

I've made it work after many attempts.

A few general considerations first:

- As a general rule to follow (valid for node.js too), you have to first make HTTPS work for WSS to work. WebSocket works over HTTP, so if your server has correctly configured HTTPS (or HTTP), then adding WebSocket to it will make WSS (or WS) work. Actually both HTTPS/WSS and HTTP/WS can work at the same time

- Certificates are very important, as not all kinds of certificates work with Jetty (the same is true for node.js). So you have to generate the certificates that Jetty will accept.

- Making HTTPS work is also important for self-signed certificates, as you might have to first access the server via HTTPS and add the exception before WSS can work (This might depend on the browser.)

- Another thing to consider is that the context paths need to be correctly setup, too. I've made it work by having separate paths for HTTP and WS, like `/hello` for HTTP(S) and `/ws` for WS(S). It might be possible to do it with the same context path for both, but I didn't investigate that yet

Below are the steps I followed.

1) Generate the correct self-signed certificates by using the commands from [here](http://stackoverflow.com/questions/4008837/configure-ssl-on-jetty)

The above link has an example on how to generate correct self-signed certificates. (If you have CA-signed certificates, the procedure is somewhat different, I think.)

I'm pasting the commands here for ease of access. Please pay attention to always give the same password whenever asked (including one of the final steps, when the password you type will appear in clear text on the screen).

<!-- language: lang-sh -->
    openssl genrsa -des3 -out jetty.key
    openssl req -new -x509 -key jetty.key -out jetty.crt
    keytool -keystore keystore -import -alias jetty -file jetty.crt -trustcacerts
    openssl req -new -key jetty.key -out jetty.csr
    openssl pkcs12 -inkey jetty.key -in jetty.crt -export -out jetty.pkcs12
    keytool -importkeystore -srckeystore jetty.pkcs12 -srcstoretype PKCS12 -destkeystore keystore

2) Have the correct code for HTTPS in Jetty.

There are some resources on the web that show how to do HTTPS with Jetty, but for me only one worked, and it is [here](http://stackoverflow.com/questions/14362245/programatically-configure-ssl-for-jetty-9-embedded).

3) Have the correct code for handling contexts.

This one was tough - the example code from the Jetty documentation page did not work for me.
What worked was [this](http://wiki.eclipse.org/Jetty/Tutorial/Embedding_Jetty#Setting_Contexts). This tutorial also enlightened me on the fact that I might have conflicts if I try to use the same path for HTTP and WS.

4) Finally, have the correct code for WebSocket

I've found correct WebSocket code [here](https://github.com/jetty-project/embedded-jetty-websocket-examples). The one that has what we need is `native-jetty-websocket-example`.
