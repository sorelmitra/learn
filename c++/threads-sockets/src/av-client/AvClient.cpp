/*
 * AvClient.cpp
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#include <dirent.h>
#include <Socket.h>
#include <iostream>

#include <google/protobuf/message.h>

#include "../common/net/defs.h"
#include "../common/utils/Utils.h"
#include "../../build/src/common/proto/AV.pb.h"

#include "AvClient.h"

AvClient::AvClient(std::string address, int port, std::string dirToScan) : socketList("Server") {
	this->address = address;
	this->port = port;
	this->directory = dirToScan;
	clientSocket = new net::ClientSocket(address, port);
	sock = NULL;
}

AvClient::~AvClient() {
	delete clientSocket;
	if (sock != NULL) {
		delete sock;
		sock = NULL;
	}
}

void AvClient::run() {
	try {
		connect();
		enumerateFiles();
		sendFileNames();
		waitForData();
	} catch (std::string& e) {
		std::cerr << e << std::endl;
	}
}

void AvClient::connect() {
	sock = clientSocket->connect();
	socketList.addSocket(sock);
}

void AvClient::enumerateFiles() {
	DIR *dir;
	struct dirent *ent;

	if ((dir = opendir(directory.c_str())) == NULL) {
		throw std::string("Could not open directory " + directory + ": " + std::string(strerror(errno)));
	}

	while ((ent = readdir(dir)) != NULL) {
		std::string dName = ent->d_name;
		if (dName == "." || dName == "..") {
			continue;
		}
		std::string *addedFileName = scanFilesList.add_filenames();
		addedFileName->append(dName);
	}

	closedir(dir);
}

void AvClient::sendFileNames() {
	utils::sendProtoBufMessage(sock, scanFilesList, "scan files list");
}

void AvClient::sendScanFileBuffer(const std::string& buffer, messages::ScanFileRequest* scanFileRequest) {
	messages::ScanFileBuffer scanFileBuffer;
	messages::ScanFileData *scanFileData = new messages::ScanFileData();
	scanFileData->set_filename(scanFileRequest->scanfiledata().filename());
	scanFileData->set_buffernr(scanFileRequest->scanfiledata().buffernr());
	scanFileData->set_offset(scanFileRequest->scanfiledata().offset());
	scanFileBuffer.set_allocated_scanfiledata(scanFileData);
	scanFileBuffer.set_buffer(buffer);
	utils::sendProtoBufMessage(sock, scanFileBuffer, "scan file buffer");
}

void AvClient::waitForData() {
	LOGINFO("Waiting for data from server");
	while (true) {
		try {
			if (socketList.getSockets().size() < 1) {
				LOGINFO("Server has disconnected, ending.");
				break;
			}
			std::list<net::Socket *> clientsWithData = socketList.pollInput();
			for (std::list<net::Socket *>::iterator it = clientsWithData.begin(); it != clientsWithData.end(); it++) {
				net::Socket *s = *it;
				LOGINFO("Server " + s->toString() + " has sent data.");
				if (processScanFileRequest(s)) {
					continue;
				}
				if (processDetectionResults(s)) {
					continue;
				}
				LOGERROR("Could not parse client data as a scan file request or detection result. Offending data is <<" + s->getLastMessage() + ">>");
			}
		} catch (std::string& e) {
			std::cerr << e << std::endl;
		}
	}
}

bool AvClient::processScanFileRequest(net::Socket *s) {
	messages::ScanFileRequest *scanFileRequest = getScanFileRequest(s);
	if (scanFileRequest != NULL) {
		sendScanFileBuffers(scanFileRequest);
		return true;
	}
	return false;
}

bool AvClient::processDetectionResults(net::Socket *s) {
	messages::DetectionResultsList *detectionResults = getDetectionResults(s);
	if (detectionResults != NULL) {
		printDetectionResults(detectionResults);
		return true;
	}
	return false;
}

void AvClient::sendScanFileBuffers(messages::ScanFileRequest* scanFileRequest) {
	const messages::ScanFileData& scanFileData =
			scanFileRequest->scanfiledata();
	std::string filePath = utils::concatenatePaths(directory,
			scanFileData.filename());
	std::string buffer = utils::readBufferFromFile(filePath,
			scanFileData.offset(), scanFileRequest->length());
	LOGINFO(
			"Read from file " + filePath + " buffer "
					+ std::to_string(scanFileData.buffernr()) + ": offset "
					+ std::to_string(scanFileData.offset()) + ", length "
					+ std::to_string(buffer.length()));
	sendScanFileBuffer(buffer, scanFileRequest);
}

void AvClient::printDetectionResults(messages::DetectionResultsList *detectionResults) {
	LOGINFO("\n\n============================");
	bool infected = false;
	std::string details;
	for (int i = 0; i < detectionResults->detectionresult_size(); i++) {
		const messages::DetectionResult &detectionResult = detectionResults->detectionresult(i);
		if (detectionResult.infected()) {
			infected = true;
			details.append("Plugin " + detectionResult.pluginname() +
					", offset " + std::to_string(detectionResult.offset()) + ". ");
		}
	}
	if (infected) {
		LOGINFO("File " + detectionResults->filename() + " is INFECTED. " + details);
	} else {
		LOGINFO("File " + detectionResults->filename() + " is CLEAN. ");
	}
	LOGINFO("============================\n");
}

messages::ScanFileRequest *AvClient::getScanFileRequest(net::Socket *s) {
	messages::ScanFileRequest *scanFileRequest = new messages::ScanFileRequest();
	bool result = scanFileRequest->ParseFromString(s->getLastMessage());
	if (!result) {
		return NULL;
	}
	LOGINFO(scanFileRequest->DebugString());
	return scanFileRequest;
}

messages::DetectionResultsList *AvClient::getDetectionResults(net::Socket *s) {
	messages::DetectionResultsList *detectionResults = new messages::DetectionResultsList();
	bool result = detectionResults->ParseFromString(s->getLastMessage());
	if (!result) {
		return NULL;
	}
	LOGINFO(detectionResults->DebugString());
	return detectionResults;
}
