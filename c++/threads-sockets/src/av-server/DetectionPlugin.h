/*
 * DetectionPlugin.h
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_DETECTIONPLUGIN_H_
#define SRC_AV_SERVER_DETECTIONPLUGIN_H_

#include <string>

#include "../build/src/common/proto/AV.pb.h"

class DetectionPlugin {
public:
	DetectionPlugin();
	virtual ~DetectionPlugin();

	virtual std::string getName() = 0;
	virtual void run(messages::ScanFileBuffer scanFileBuffer, messages::DetectionResult* detectionResult) = 0;
};

#endif /* SRC_AV_SERVER_DETECTIONPLUGIN_H_ */
