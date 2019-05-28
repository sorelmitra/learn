#ifndef CLIENTSOCKET_H
#define CLIENTSOCKET_H

#include <string>

#include "Socket.h"

namespace net {
    class ClientSocket {

    protected:
        std::string address;
        int port;
        int socketfd;
        
    public:
        ClientSocket(std::string hostname, int port) {
            this->address = hostname;
            this->port = port;
            this->socketfd = -1;
        }
        
        net::Socket *connect();
    };
}

#endif // CLIENTSOCKET_H

