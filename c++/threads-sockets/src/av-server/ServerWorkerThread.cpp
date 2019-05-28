/*
 * ServerWorkerThread.cpp
 *
 *  Created on: Nov 16, 2017
 *      Author: sorel
 */

#include <iostream>
#include <algorithm>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "../common/net/defs.h"
#include "../common/utils/Utils.h"
#include "DetectionPluginText.h"

#include "ServerWorkerThread.h"

ServerWorkerThread::ServerWorkerThread(int nr, ServerDetectionPlugins &x) : ServerCommonThread(), detectionPlugins(x) {
	this->nr = nr;
	socketList = new net::SocketList(getName() + " Client");
	LOGINFO(getName() + " started, detection plugins: " + detectionPlugins.getNames());
}

ServerWorkerThread::~ServerWorkerThread() {
	stop();
	delete socketList;
}

void ServerWorkerThread::addClientFile(net::Socket *client, std::string fileName) {
	ClientFileData *clientFileData = new ClientFileData(client, fileName);
	clientFiles.push_back(clientFileData);
	LOGINFO(getName() + ": Added client " + client->toString() + " file " + fileName);
	socketStatuses[client]->filesToScan++;
}

void ServerWorkerThread::addClient(net::Socket *client) {
	std::unique_lock<std::mutex> lock(mtx);
	socketList->addSocket(client);
	if (socketStatuses[client] == NULL) {
		socketStatuses[client] = new SocketStatus();
		socketStatuses[client]->waitingForScanFiles = true;
	}
}

bool ServerWorkerThread::workABit() {
	std::unique_lock<std::mutex> lock(mtx);
	requestBuffersFromClients();
	cleanupClientFiles();
	getBuffersFromClients();
	return true;
}

void ServerWorkerThread::getScanFiles(net::Socket *client) {
	messages::ScanFilesList *scanFilesList = getScanFilesList(client);
	if (scanFilesList == NULL) {
		return;
	}
	std::list<std::string> orderedFileNames = getOrderedFileNames(scanFilesList);
	for (std::list<std::string>::iterator it2 = orderedFileNames.begin(); it2 != orderedFileNames.end(); it2++) {
		std::string fileName = *it2;
		addClientFile(client, fileName);
	}
	socketStatuses[client]->waitingForScanFiles = false;
}

messages::ScanFilesList *ServerWorkerThread::getScanFilesList(net::Socket *client) {
	messages::ScanFilesList *scanFilesList = new messages::ScanFilesList();
	bool result = scanFilesList->ParseFromString(client->getLastMessage());
	if (!result) {
		return NULL;
	}
	LOGINFO(scanFilesList->DebugString());
	return scanFilesList;
}

std::list<std::string> ServerWorkerThread::getOrderedFileNames(messages::ScanFilesList *scanFilesList) {
	std::list<std::string> orderedFileNames;
	int n = scanFilesList->filenames_size();
	for (int i = 0; i < n; i++) {
		std::string fileName = scanFilesList->filenames(i);
		std::string ext = boost::filesystem::extension(fileName);
		boost::algorithm::to_lower(ext);
		if (ext == ".txt" || ext == ".bin") {
			orderedFileNames.push_front(fileName);
		} else {
			orderedFileNames.push_back(fileName);
		}
	}
	return orderedFileNames;
}

void ServerWorkerThread::requestBuffersFromClients() {
	for (std::list<ClientFileData*>::iterator it = clientFiles.begin(); it != clientFiles.end(); it++) {
		ClientFileData* clientFileData = *it;
		if (socketStatuses[clientFileData->client]->waitingForBuffers) {
			continue;
		}
		if (clientFileData->gotAllBuffers) {
			LOGINFO(
					"Got entire file " + clientFileData->fileName
							+ " from client "
							+ clientFileData->client->toString());
			continue;
		}
		sendScanFileRequest(clientFileData);
	}
}

void ServerWorkerThread::cleanupClientFiles() {
	for (std::list<ClientFileData*>::iterator it = clientFiles.begin(); it != clientFiles.end(); ) {
		ClientFileData* clientFileData = *it;
		net::Socket *client = clientFileData->client;
		if (clientFileData->gotAllBuffers) {
			LOGINFO("Removing file " + clientFileData->fileName + " for client " + client->toString());
			socketStatuses[client]->waitingForBuffers = false;
			socketStatuses[client]->filesToScan--;
			if (socketStatuses[client]->filesToScan < 1) {
				LOGINFO("Scanned all files from client " + client->toString() + ", removing it");
				socketStatuses.erase(client);
				socketList->removeSocket(client);
				client->close();
			}
			clientFiles.erase(it++);
		} else {
			++it;
		}
	}
}

void ServerWorkerThread::getBuffersFromClients() {
	std::list<net::Socket*> clientsWithData = socketList->pollInput();
	for (std::list<net::Socket*>::iterator it = clientsWithData.begin();
			it != clientsWithData.end(); it++) {
		net::Socket* client = *it;
		if (socketStatuses[client]->waitingForScanFiles) {
			getScanFiles(client);
			continue;
		}
		messages::ScanFileBuffer scanFileBuffer;
		bool result = scanFileBuffer.ParseFromString(client->getLastMessage());
		if (!result) {
			LOGERROR(
					"Could not parse scan file buffer from client "
							+ client->toString() + "!");
			continue;
		}
		const messages::ScanFileData &scanFileData = scanFileBuffer.scanfiledata();
		markClientFileDataGotAllBuffers(scanFileBuffer);
		LOGINFO(
				"Read from client " + client->toString() + " file " + scanFileData.filename() + " buffer "
						+ std::to_string(scanFileData.buffernr())
						+ ": offset "
						+ std::to_string(scanFileData.offset())
						+ ", length " + std::to_string(scanFileBuffer.buffer().length()));
		messages::DetectionResultsList detectionResults = detectionPlugins.runAll(
				scanFileBuffer);
		sendDetectionResults(client, detectionResults);
	}
}

bool ServerWorkerThread::markClientFileDataGotAllBuffers(const messages::ScanFileBuffer& scanFileBuffer) {
	const messages::ScanFileData &scanFileData = scanFileBuffer.scanfiledata();
	bool found = false;
	for (std::list<ClientFileData*>::iterator it2 = clientFiles.begin();
			it2 != clientFiles.end(); it2++) {
		ClientFileData* clientFileData = *it2;
		if (clientFileData->fileName == scanFileData.filename()) {
			clientFileData->gotAllBuffers = true;
			found = true;
		}
	}
	if (!found) {
		LOGERROR(
				"Could not find client file data for file  "
						+ scanFileData.filename());
	}
	return found;
}

void ServerWorkerThread::sendScanFileRequest(ClientFileData *clientFileData) {
	net::Socket *client = clientFileData->client;
	std::string output;
	messages::ScanFileRequest scanFileRequest;
	messages::ScanFileData *scanFileData = new messages::ScanFileData();
	scanFileData->set_filename(clientFileData->fileName);
	scanFileData->set_buffernr(1);
	scanFileData->set_offset(clientFileData->lastOffsetRead + 1);
	scanFileRequest.set_length(clientFileData->bufferSize);
	scanFileRequest.set_allocated_scanfiledata(scanFileData);
	utils::sendProtoBufMessage(client, scanFileRequest, "request for file to scan " + scanFileData->filename());
	socketStatuses[client]->waitingForBuffers = true;
	LOGINFO("Waiting for client " + client->toString() + " to send buffers for file " + clientFileData->fileName);
}

void ServerWorkerThread::sendDetectionResults(net::Socket *client, messages::DetectionResultsList detectionResults) {
	utils::sendProtoBufMessage(client, detectionResults, "detection results for " + detectionResults.filename());
}

