/*
 * clientsocketlist.h
 *
 *  Created on: Nov 24, 2017
 *      Author: sorel
 */

#ifndef SRC_COMMON_NET_SOCKETLIST_H_
#define SRC_COMMON_NET_SOCKETLIST_H_

#include <list>

#include "Socket.h"

namespace net {
	class SocketList {
	private:
		std::list<net::Socket *> sockets;
		std::string category;

	public:
		SocketList(std::string cat);
		virtual ~SocketList();

		void addSocket(net::Socket* socket);
		void removeSocket(net::Socket* socket);
		std::list<net::Socket *> pollInput();

		std::list<net::Socket *> getSockets() {
			return sockets;
		}

	private:
		struct pollfd *prepareFileDescriptors();
		void fillClientsWithData(std::list<net::Socket *> &clientsWithData, struct pollfd *ufds);
	};
}

#endif /* SRC_COMMON_NET_SOCKETLIST_H_ */
