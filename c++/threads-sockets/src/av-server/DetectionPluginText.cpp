/*
 * DetectionPluginText.cpp
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#include <iostream>
#include <fstream>

#include "../common/net/defs.h"

#include "DetectionPluginText.h"

DetectionPluginText::DetectionPluginText(std::string configFilePath) {
	this->configFilePath = configFilePath;
	std::ifstream myfile(configFilePath);
	std::string line;
	while (std::getline(myfile, line)) {
		sequences.push_back(line);
	}
}

DetectionPluginText::~DetectionPluginText() {
}

void DetectionPluginText::run(messages::ScanFileBuffer scanFileBuffer, messages::DetectionResult* detectionResult) {
	detectionResult->set_pluginname(getName());
	const std::string &toScan = scanFileBuffer.buffer();
	for (std::list<std::string>::iterator it = sequences.begin(); it != sequences.end(); it++) {
		std::string seq = *it;
		int offset = toScan.find(seq);
		if (offset != std::string::npos) {
			detectionResult->set_infected(true);
			detectionResult->set_offset(offset);
			LOGINFO("INFECTED file " + scanFileBuffer.scanfiledata().filename() +
					": found sequence <<" + seq + ">> at offset " + std::to_string(offset));
			break;
		}
	}
}
