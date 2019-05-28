#ifndef SERVERSOCKET_H
#define SERVERSOCKET_H

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>
#include <cerrno>

#include "Socket.h"

namespace net {
	class ServerSocket {
		protected:
			int port;
			int backlog;
			std::string address;

			int socketFd;

		public:
			ServerSocket(int port) {
				this->port = port;
				backlog = 10;
				address = "0.0.0.0";
	            this->socketFd = -1;
			}

			ServerSocket(int port, int backlog) {
				this->port = port;
				this->backlog = backlog;
				address = "0.0.0.0";
	            this->socketFd = -1;
			}

			ServerSocket(int port, int backlog, std::string address) {
				this->port = port;
				this->backlog = backlog;
				this->address = address;
	            this->socketFd = -1;
			}

			~ServerSocket();

			int listen();

			net::Socket* accept();

			void close() {
				if (socketFd == -1) {
					return;
				}

				::close(socketFd);
			}

			bool isValid() {
				return socketFd != -1;
			}

			int getSocket() {
				return socketFd;
			}
	};
};

#endif
