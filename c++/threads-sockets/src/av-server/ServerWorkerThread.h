/*
 * ServerWorkerThread.h
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_SERVERWORKERTHREAD_H_
#define SRC_AV_SERVER_SERVERWORKERTHREAD_H_

#include <list>
#include <map>
#include <mutex>

#include "../common/net/SocketList.h"
#include "../common/net/Socket.h"
#include "../build/src/common/proto/AV.pb.h"
#include "../common/net/ServerSocket.h"
#include "ServerCommonThread.h"
#include "ClientFileData.h"
#include "ServerDetectionPlugins.h"
#include "SocketStatus.h"

class ServerWorkerThread : ServerCommonThread {
private:
	net::SocketList *socketList;
	std::list<ClientFileData *> clientFiles;
	std::map<net::Socket *, SocketStatus *> socketStatuses;
	std::mutex mtx;
	ServerDetectionPlugins &detectionPlugins;
	int nr;

public:
	ServerWorkerThread(int nr, ServerDetectionPlugins &detectionPlugins);
	virtual ~ServerWorkerThread();

	void addClient(net::Socket *client);

	std::string getName() {
		return "Worker Thread " + std::to_string(nr);
	}

protected:
	virtual bool workABit();

private:
	void getScanFiles(net::Socket *client);
	messages::ScanFilesList *getScanFilesList(net::Socket *client);
	std::list<std::string> getOrderedFileNames(messages::ScanFilesList *scanFilesList);
	void addClientFile(net::Socket *, std::string);
	void sendScanFileRequest(ClientFileData *);
	void sendDetectionResults(net::Socket *client, messages::DetectionResultsList detectionResults);
	void requestBuffersFromClients();
	void getBuffersFromClients();
	void cleanupClientFiles();
	bool markClientFileDataGotAllBuffers(const messages::ScanFileBuffer& scanFileBuffer);
};

#endif /* SRC_AV_SERVER_SERVERWORKERTHREAD_H_ */
