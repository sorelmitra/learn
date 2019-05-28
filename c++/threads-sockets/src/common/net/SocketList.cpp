/*
 * clientsocketlist.cpp
 *
 *  Created on: Nov 24, 2017
 *      Author: sorel
 */

#include <sys/poll.h>
#include <list>
#include <algorithm>

#include "../common/net/defs.h"

#include "SocketList.h"

net::SocketList::SocketList(std::string cat) : category(cat) {
}

net::SocketList::~SocketList() {
}

void net::SocketList::addSocket(net::Socket *socket) {
	sockets.insert(sockets.end(), socket);
	net::SocketAddress* addr = socket->getSocketAddress();
	LOGINFO("Added " + category + " " << addr->getInnerAddress() << ":" << addr->getPort() << ", FD " << std::to_string(socket->getSocketFd()));
}

void net::SocketList::removeSocket(net::Socket *socket) {
	net::SocketAddress* addr = socket->getSocketAddress();
	LOGINFO("" + category + " " << addr->getInnerAddress() << ":" << addr->getPort() << ", FD " << std::to_string(socket->getSocketFd()) << " disconnected");
	sockets.remove(socket);
}

std::list<net::Socket *> net::SocketList::pollInput() {
	std::list<net::Socket *> clientsWithData;
	if (sockets.size() < 1) {
		return clientsWithData;
	}

	struct pollfd *ufds = prepareFileDescriptors();
	int result = poll(ufds, sockets.size(), 100);

	if (result == -1) {
		LOGINFO("net::socketlist::pollInput" << strerror(errno));
		return clientsWithData;
	}

	if (result == 0) {
		return clientsWithData;
	}

	fillClientsWithData(clientsWithData, ufds);
	delete[] ufds;
	return clientsWithData;
}

struct pollfd *net::SocketList::prepareFileDescriptors() {
	struct pollfd *ufds = new struct pollfd[sockets.size()];
	int i;
	std::list<net::Socket *>::iterator it;
	for (i = 0, it = sockets.begin(); (it != sockets.end()) && (i < sockets.size()); it++, i++) {
		net::Socket *client = *it;
		ufds[i].fd = client->getSocketFd();
		ufds[i].events = POLLIN | POLLPRI;
		ufds[i].revents = 0;
	}
	return ufds;
}

void net::SocketList::fillClientsWithData(std::list<net::Socket *> &clientsWithData, struct pollfd *ufds) {
	int i;
	std::list<net::Socket *>::iterator it;
	for (i = 0, it = sockets.begin(); (it != sockets.end()) && (i < sockets.size()); it++, i++) {
		bool gotData = false;
		net::Socket *client = *it;
		if (ufds[i].revents & POLLIN) {
			gotData = true;
		}
		if (ufds[i].revents & POLLPRI) {
			gotData = true;
		}
		std::string msg;
		if (!gotData) {
			continue;
		}
		client->read(msg);
		if (msg.length() < 1) {
			removeSocket(client);
			continue;
		}
		client->setLastMessage(msg);
		clientsWithData.insert(clientsWithData.end(), client);
		LOGINFO("Read " + std::to_string(msg.length()) + " bytes into " + client->toString() + ", list length " + std::to_string(clientsWithData.size()));
	}
}

