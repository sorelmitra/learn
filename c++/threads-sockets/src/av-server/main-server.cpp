#include <iostream>
#include <unistd.h>
#include <cstdlib>
#include <string>
#include <iostream>
#include <cstdio>

#include "../common/net/defs.h"

#include "AvServer.h"

int main(int argc, char * const argv[]) {
	int c;
	int threadsCount = 1;
	while ((c = getopt(argc, argv, "t:")) != -1) {
		switch (c) {
		case 't':
			threadsCount = atoi(optarg);
			break;
		case '?':
			if (optopt == 't')
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
	if (threadsCount < 1) {
		LOGERROR("Need at least one thread, please use -t argument");
		return 1;
	}
	if (threadsCount > 20) {
		LOGERROR("Maximum 20 threads can run, please correct -t argument");
		return 1;
	}

	LOGINFO("Using " + std::to_string(threadsCount) + " threads");

	AvServer avServer(net::SERVER_PORT, threadsCount);
	avServer.listen();
	return 0;
}
