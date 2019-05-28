#include <ServerSocket.h>

net::ServerSocket::~ServerSocket() {
	close();
}

int net::ServerSocket::listen() {

	net::SocketAddress sockaddr(address, port);
	struct sockaddr_in addr = sockaddr.getInnerStruct();

	socketFd = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	if (socketFd == -1) {
		return errno;
	}

	int yes = 1;
	if (::setsockopt(socketFd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) != 0) {
		close();
		return errno;
	}

	if (::bind(socketFd, (struct sockaddr*)&addr, sizeof(struct sockaddr)) != 0) {
		close();
		return errno;
	}

	if (::listen(socketFd, backlog) != 0) {
		close();
		return errno;
	}

	return 0;
}

net::Socket* net::ServerSocket::accept() {
	struct sockaddr_in from;
	socklen_t l = sizeof(from);
	int clientfd = ::accept(socketFd, (struct sockaddr*)&from, &l);

	return new net::Socket(clientfd, from);
}
