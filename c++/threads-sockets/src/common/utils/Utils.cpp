/*
 * Utils.cpp
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#include <iostream>
#include <fstream>
#include <algorithm>

#include "../common/net/defs.h"

#include "Utils.h"

void utils::sendProtoBufMessage(net::Socket *sock, ::google::protobuf::Message &protobufMessage, std::string name) {
	LOGINFO("\n\nSending " + name + " to " + sock->toString());
	//LOGINFO(protobufMessage.DebugString());
	std::string output;
	bool result = protobufMessage.SerializeToString(&output);
	if (!result) {
		throw std::string("Could not serialize the " + name + " to string!");
	}

	int sendResult = sock->send(output);
	if (sendResult < 0) {
		throw std::string("Could not send the " + name + ": " + std::string(strerror(errno)));
	}
	LOGINFO("Sent " + name + " to " + sock->toString());
}

std::string utils::readBufferFromFile(const std::string &filePath, int offset, int length) {
	// Open file
	std::ifstream file(filePath, std::ios::in | std::ios::binary | std::ios::ate);
	if (!file.is_open()) {
		throw std::string("Could not open file " + filePath + "!");
	}

	// Check offset limits
	std::streampos fileSize = file.tellg();
	if (offset < 0) {
		throw std::string("Bad offset " + std::to_string(offset) + ": should be > " + std::to_string(std::ios::beg));
	}
	if (offset >= fileSize) {
		throw std::string("Bad offset " + std::to_string(offset) + ": should be < " + std::to_string(fileSize));
	}

	// Determine buffer size
	int availableLength = (int)fileSize - offset;
	int size;
	if (length < 0) {
		size = availableLength;
	} else {
		size = std::min((int)length, availableLength);
	}

	// Allocate memblock
	char *memblock = new char[size];

	// Read in memblock
	file.seekg(offset, std::ios::beg);
	file.read(memblock, size);
	file.close();

	// Copy memblock to buffer
	std::string buffer(memblock, size);
	delete[] memblock;

	return buffer;
}

std::string utils::concatenatePaths(std::string p1, std::string p2) {
	return p1 + "/" + p2;
}

