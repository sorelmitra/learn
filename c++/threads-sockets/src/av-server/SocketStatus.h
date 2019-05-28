/*
 * SocketStatus.h
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_SOCKETSTATUS_H_
#define SRC_AV_SERVER_SOCKETSTATUS_H_

class SocketStatus {
public:
	bool waitingForScanFiles;
	bool waitingForBuffers;
	int filesToScan;

	SocketStatus() {
		waitingForScanFiles = false;
		waitingForBuffers = false;
		filesToScan = 0;
	}
};

#endif /* SRC_AV_SERVER_SOCKETSTATUS_H_ */
