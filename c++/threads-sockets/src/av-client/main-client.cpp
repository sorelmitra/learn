//
//  main.cpp
//  av-client
//
//  Created by Sorel Mitra on 15/11/17.
//  Copyright © 2017 Sorel Mitra. All rights reserved.
//

#include <iostream>
#include <unistd.h>
#include <cstdlib>
#include <string>
#include <iostream>
#include <cstdio>

#include "../common/net/defs.h"

#include "AvClient.h"

int main(int argc, char * const argv[]) {
	int c;
	char *dataDir = NULL;
	while ((c = getopt(argc, argv, "d:")) != -1) {
		switch (c) {
		case 'd':
			dataDir = optarg;
			break;
		case '?':
			if (optopt == 'd')
				fprintf(stderr, "Option -%c requires an argument.\n", optopt);
			else if (isprint(optopt))
				fprintf(stderr, "Unknown option `-%c'.\n", optopt);
			else
				fprintf(stderr, "Unknown option character `\\x%x'.\n", optopt);
			return 1;
		default:
			abort();
		}
	}

	for (int i = optind; i < argc; i++) {
		LOGINFO("Non-option argument " + std::string(argv[i]));
	}
	if (optind < argc) {
		LOGERROR("Trailing arguments!");
		return 1;
	}

	if (dataDir == NULL) {
		LOGERROR("Need directory to scan. Use -d argument.");
		return 1;
	}

	std::string dirToScan = std::string(dataDir);
	LOGINFO("Scanning " + dirToScan + " directory");

	AvClient avClient("localhost", net::SERVER_PORT, dirToScan);
	avClient.run();
    return 0;
}
