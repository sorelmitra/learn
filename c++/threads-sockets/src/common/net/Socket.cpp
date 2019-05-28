#include <string>
#include <fcntl.h>
#include <Socket.h>

#define DEFAULT_SOCKET_BUFFER 128

net::Socket::~Socket() {
	delete socketAddress;
	close();
}

void net::Socket::setBlocking() {
	int opts = fcntl(socketFd, F_GETFL);
	opts = opts & (~O_NONBLOCK);
	fcntl(socketFd, F_SETFL, opts);
}

void net::Socket::setUnblocking() {
	fcntl(socketFd, F_SETFL, O_NONBLOCK);
}

int net::Socket::read(std::string& msg) {
	int bytes_total = 0;
	char buffer[DEFAULT_SOCKET_BUFFER];

	int bytes_read = recv(socketFd, buffer, DEFAULT_SOCKET_BUFFER, 0);

	if (bytes_read <= 0) {
		return bytes_read;
	}

	msg.append(std::string(buffer, bytes_read));
	bytes_total += bytes_read;

	// set non-blocking.
	setUnblocking();

	while (bytes_read > 0) {
		memset(buffer, 0, DEFAULT_SOCKET_BUFFER);
		bytes_read = recv(socketFd, buffer, DEFAULT_SOCKET_BUFFER, 0);

		if (bytes_read < 0) {
			break;
		}

		msg.append(std::string(buffer, bytes_read));
		bytes_total += bytes_read;
	}

	// set back to blocking
	setBlocking();

	return bytes_total;
}

int net::Socket::read(char* buf, int len) {
	return ::recv(socketFd, buf, len, 0);
}

int net::Socket::send(std::string data) {
	return send(data.c_str(), data.length(), 0);
}

int net::Socket::send(const char* buf, int len, int flags) {
	return ::send(socketFd, buf, len, flags);
}
