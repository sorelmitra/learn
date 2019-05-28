/*
 * Utils.h
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#ifndef SRC_COMMON_UTILS_UTILS_H_
#define SRC_COMMON_UTILS_UTILS_H_

#include <string>

#include "../net/Socket.h"

#include <google/protobuf/message.h>


namespace utils {

	void sendProtoBufMessage(net::Socket *sock, ::google::protobuf::Message &protobufMessage, std::string name);
	std::string readBufferFromFile(const std::string &filePath, int offset, int length);
	std::string concatenatePaths(std::string p1, std::string p2);
}



#endif /* SRC_COMMON_UTILS_UTILS_H_ */
