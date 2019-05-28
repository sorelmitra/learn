/*
 * AvServer.h
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_AVSERVER_H_
#define SRC_AV_SERVER_AVSERVER_H_

#include <list>

#include "../common/net/ServerSocket.h"
#include "../common/net/Socket.h"
#include "ServerWorkerThread.h"

class AvServer {
private:
	net::ServerSocket* server;
	int port;
	std::list<ServerWorkerThread *> workingThreads;
	std::list<ServerWorkerThread *>::iterator currentWorkerThreadIt;
	ServerDetectionPlugins detectionPlugins;

public:
	AvServer(int port, int threadsCount);
	virtual ~AvServer();

	void listen();
};

#endif /* SRC_AV_SERVER_AVSERVER_H_ */
