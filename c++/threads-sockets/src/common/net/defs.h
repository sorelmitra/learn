/*
 * defs.h
 *
 *  Created on: Nov 15, 2017
 *      Author: sorel
 */

#ifndef SRC_COMMON_NET_DEFS_H_
#define SRC_COMMON_NET_DEFS_H_

#include <iostream>

namespace net {
	const int SERVER_PORT = 35000;
}

#define LOGINFO(streamChain) (std::cout << streamChain << std::endl << std::flush)
#define LOGERROR(streamChain) (std::cerr << "ERROR: " << streamChain << std::endl << std::flush)
#define LOGDEBUG(streamChain) (std::cout << "DEBUG: " << streamChain << std::endl << std::flush)

#endif /* SRC_COMMON_NET_DEFS_H_ */
