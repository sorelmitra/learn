#ifndef SOCKET_ADDRESS_H
#define SOCKET_ADDRESS_H

#include <netinet/in.h>
#include <arpa/inet.h>
#include <string>
#include <cstring>

namespace net {
	class SocketAddress {
		protected:
			int port;
			std::string address;

		public:
			SocketAddress(struct sockaddr_in addr) {
				port = addr.sin_port;

				char ip[INET_ADDRSTRLEN];
				inet_ntop(addr.sin_family, &(addr.sin_addr), ip, INET_ADDRSTRLEN);

				address = std::string(ip);
			}

			SocketAddress(std::string address, int port) {
				this->address = address;
				this->port = port;
			}

			struct sockaddr_in getInnerStruct() {
				struct sockaddr_in addr;
				memset(&addr, 0, sizeof addr);

				addr.sin_family = AF_INET;
				addr.sin_port = htons(port);

				inet_aton(address.c_str(), &addr.sin_addr);

				return addr;
			}

			int getPort() {
				return port;
			}

			std::string getInnerAddress() {
				return address;
			}
	};
};
#endif
