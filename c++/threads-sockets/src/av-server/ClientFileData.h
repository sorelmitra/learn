/*
 * ClientFileData.h
 *
 *  Created on: Nov 24, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_CLIENTFILEDATA_H_
#define SRC_AV_SERVER_CLIENTFILEDATA_H_

#include <string>

#include "../common/net/Socket.h"

class ClientFileData {
public:
	net::Socket *client;
	std::string fileName;
	int lastOffsetRead;
	int bufferSize;
	bool gotAllBuffers;
	std::string buffer;

	ClientFileData(net::Socket *client, std::string fileName) {
		this->client = client;
		this->fileName = fileName;
		lastOffsetRead = -1;
		bufferSize = -1;
		gotAllBuffers = false;
	}
};

#endif /* SRC_AV_SERVER_CLIENTFILEDATA_H_ */
