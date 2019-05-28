/*
 * AvClient.h
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_CLIENT_AVCLIENT_H_
#define SRC_AV_CLIENT_AVCLIENT_H_

#include <list>
#include <string>

#include "../common/net/ClientSocket.h"
#include "../build/src/common/proto/AV.pb.h"
#include "../common/net/SocketList.h"


class AvClient {
private:
	std::string address;
	int port;
	net::ClientSocket *clientSocket;
	net::Socket *sock;
	std::string directory;
	messages::ScanFilesList scanFilesList;
	net::SocketList socketList;

public:
	AvClient(std::string address, int port, std::string dirToScan);
	virtual ~AvClient();

	void run();

private:
	void connect();
	void enumerateFiles();
	void sendFileNames();
	void waitForData();
	bool processScanFileRequest(net::Socket *s);
	bool processDetectionResults(net::Socket *s);
	messages::ScanFileRequest *getScanFileRequest(net::Socket *s);
	messages::DetectionResultsList *getDetectionResults(net::Socket *s);
	void printDetectionResults(messages::DetectionResultsList *detectionResults);
	void sendScanFileBuffer(const std::string& buffer, messages::ScanFileRequest* scanFileRequest);
	void sendScanFileBuffers(messages::ScanFileRequest* scanFileRequest);
};

#endif /* SRC_AV_CLIENT_AVCLIENT_H_ */
