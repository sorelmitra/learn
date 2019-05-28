/*
 * DetectionPluginText.h
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#ifndef SRC_AV_SERVER_DETECTIONPLUGINTEXT_H_
#define SRC_AV_SERVER_DETECTIONPLUGINTEXT_H_

#include <list>
#include <string>

#include "DetectionPlugin.h"

class DetectionPluginText : public DetectionPlugin {
private:
	std::string configFilePath;
	std::list<std::string> sequences;

public:
	DetectionPluginText(std::string configFilePath);
	virtual ~DetectionPluginText();

	virtual std::string getName() {
		return "TEXT";
	}

	virtual void run(messages::ScanFileBuffer scanFileBuffer, messages::DetectionResult* detectionResult);
};

#endif /* SRC_AV_SERVER_DETECTIONPLUGINTEXT_H_ */
