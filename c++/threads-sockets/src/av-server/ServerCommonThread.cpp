/*
 * ServerCommonThread.cpp
 *
 *  Created on: Nov 24, 2017
 *      Author: sorel
 */


#include <iostream>
#include <chrono>

#include "../common/net/defs.h"

#include "ServerCommonThread.h"

ServerCommonThread::ServerCommonThread() {
	thread = std::thread(&ServerCommonThread::worker, this);
	stopRequested = false;
}

ServerCommonThread::~ServerCommonThread() {
	stop();
}

void ServerCommonThread::stop() {
	stopRequested = true;
	thread.join();
}

void ServerCommonThread::worker() {
	while (true) {
		if (stopRequested) {
			break;
		}
		workABit();
		std::this_thread::sleep_for(std::chrono::milliseconds(100));
	}
}
