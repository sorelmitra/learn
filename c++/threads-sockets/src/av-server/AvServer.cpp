/*
 * AvServer.cpp
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#include <iostream>

#include "../common/net/defs.h"
#include "ServerWorkerThread.h"
#include "DetectionPluginText.h"

#include "AvServer.h"

AvServer::AvServer(int port, int threadsCount) {
	this->port = port;
	server = new net::ServerSocket(port);
	detectionPlugins.add(new DetectionPluginText("config/plugin-scan-text.txt"));
	for (int i = 0; i < threadsCount; i++) {
		workingThreads.push_back(new ServerWorkerThread(i + 1, detectionPlugins));
	}
	currentWorkerThreadIt = workingThreads.begin();
}

AvServer::~AvServer() {
	delete server;
}

void AvServer::listen() {
	int err = server->listen();

	if (err != 0) {
		LOGINFO(strerror(err));
		exit(err);
	}

	LOGINFO("Server started on port " + std::to_string(port));

	while (true) {
		net::Socket* client = server->accept();

		if (!client->isValid()) {
			delete client;
			continue;
		}
		ServerWorkerThread *workerThread = *currentWorkerThreadIt;
		workerThread->addClient(client);

		if (++currentWorkerThreadIt == workingThreads.end()) {
			currentWorkerThreadIt = workingThreads.begin();
		}
	}
}
