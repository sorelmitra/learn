FROM postgres:latest
LABEL "Product"="PostgreSQL (SSL Certificate Enabled)"
COPY ssl/server.key /var/lib/postgresql/server.key
COPY ssl/server.crt /var/lib/postgresql/server.crt
COPY ssl/SorelCA.pem /var/lib/postgresql/root.crt
RUN chown postgres /var/lib/postgresql/server.key && \
    chmod 600 /var/lib/postgresql/server.key
