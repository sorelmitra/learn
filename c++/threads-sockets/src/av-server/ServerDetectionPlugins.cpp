/*
 * ServerDetectionPlugins.cpp
 *
 *  Created on: Nov 27, 2017
 *      Author: sorel
 */

#include "ServerDetectionPlugins.h"

ServerDetectionPlugins::ServerDetectionPlugins() {
}

ServerDetectionPlugins::~ServerDetectionPlugins() {
}

void ServerDetectionPlugins::add(DetectionPlugin *plugin) {
	detectionPlugins.push_back(plugin);
}

messages::DetectionResultsList ServerDetectionPlugins::runAll(messages::ScanFileBuffer scanFileBuffer) {
	messages::DetectionResultsList detectionResults;

	detectionResults.set_filename(scanFileBuffer.scanfiledata().filename());

	messages::DetectionResult* detectionResult = NULL;

	for (std::list<DetectionPlugin *>::iterator it = detectionPlugins.begin(); it != detectionPlugins.end(); it++) {
		DetectionPlugin *plugin = *it;
		detectionResult = detectionResults.add_detectionresult();
		plugin->run(scanFileBuffer, detectionResult);
	}

	return detectionResults;
}

std::string ServerDetectionPlugins::getNames() {
	std::string names;
	for (std::list<DetectionPlugin *>::iterator it = detectionPlugins.begin(); it != detectionPlugins.end(); it++) {
		DetectionPlugin *plugin = *it;
		names.append(plugin->getName());
	}
	return names;
}
