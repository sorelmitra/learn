#include <ClientSocket.h>
#include <SocketAddress.h>

net::Socket *net::ClientSocket::connect() {
	net::SocketAddress sockaddr(address, port);
	struct sockaddr_in addr = sockaddr.getInnerStruct();

	socketfd = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	net::Socket *sock = new net::Socket(socketfd, addr);

	int res = ::connect(socketfd, (struct sockaddr*) (&addr), sizeof(addr));
	if (res < 0) {
		throw "Could not connect to " + address + ":" + std::to_string(port);
	}

	return sock;
}
