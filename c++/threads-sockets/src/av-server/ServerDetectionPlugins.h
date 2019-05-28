/*
 * ServerDetectionPlugins.h
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_SERVERDETECTIONPLUGINS_H_
#define SRC_AV_SERVER_SERVERDETECTIONPLUGINS_H_

#include <list>

#include "../build/src/common/proto/AV.pb.h"
#include "DetectionPlugin.h"

class ServerDetectionPlugins {
private:
	std::list<DetectionPlugin *> detectionPlugins;

public:
	ServerDetectionPlugins();
	virtual ~ServerDetectionPlugins();

	void add(DetectionPlugin *plugin);
	std::string getNames();
	messages::DetectionResultsList runAll(messages::ScanFileBuffer scanFileBuffer);
};

#endif /* SRC_AV_SERVER_SERVERDETECTIONPLUGINS_H_ */
