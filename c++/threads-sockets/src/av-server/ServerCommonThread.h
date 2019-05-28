/*
 * ServerCommonThread.h
 *
 *  Created on: Nov 24, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_SERVERCOMMONTHREAD_H_
#define SRC_AV_SERVER_SERVERCOMMONTHREAD_H_

#include <thread>

#include "../common/net/Socket.h"

class ServerCommonThread {
private:
	std::thread thread;
	int stopRequested;
public:
	ServerCommonThread();
	virtual ~ServerCommonThread();

protected:
	void worker();
	void stop();

	virtual bool workABit() = 0;
};

#endif /* SRC_AV_SERVER_SERVERCOMMONTHREAD_H_ */
