#ifndef SOCKET_H
#define SOCKET_H

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <string>
#include <unistd.h>
#include <cerrno>

#include "SocketAddress.h"

namespace net {
	class Socket {
		protected:
			int socketFd;
			struct sockaddr_in address;
			SocketAddress* socketAddress;
			std::string lastMessage;

		public:
			Socket(int socket, struct sockaddr_in addr) {
				socketFd = socket;
				address = addr;

				socketAddress = new SocketAddress(addr);
			}

			~Socket();

			int read(std::string&);
			int read(char*, int);

			int send(std::string);
			int send(const char*, int len, int flags);

			void setBlocking();
			void setUnblocking();

			void close() {
				if (socketFd == -1) {
					return;
				}

				::close(socketFd);
			}

			bool isValid() {
				return socketFd != -1;
			}

			int getSocketFd() {
				return socketFd;
			}

			SocketAddress* getSocketAddress() {
				return socketAddress;
			}

			void setLastMessage(std::string msg) {
				lastMessage = msg;
			}

			std::string getLastMessage() {
				return lastMessage;
			}

			std::string toString() {
				return std::string("") +
						socketAddress->getInnerAddress() + ":" + std::to_string(socketAddress->getPort()) +
						", FD " + std::to_string(getSocketFd());
			}
	};
};

#endif
