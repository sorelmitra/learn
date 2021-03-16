import { sleep } from 'k6';

import {createFoundation, destroyFoundation} from '../utils/foundation.js';
import { defaultOrEnv, FIELD_VARIABLES_GLOBAL_VALUES, logServer } from '../utils/utils.js';

let waitSecondsBeforeDestroy = 1;

export function setup() {
	logServer();

	let data = {
		createOnly: defaultOrEnv(false, "K6S_CREATE_ONLY"),
		tag: defaultOrEnv("", "K6S_TAG"),
	};

	return data;
}

export default function smokeTest(data) {
	FIELD_VARIABLES_GLOBAL_VALUES.TAG = data.tag;
	let foundation = createFoundation();
	if (data.createOnly) {
		console.log(`DEBUG: We're being told to create only, skipping destroy foundation.`);
		return;
	}
	sleep(waitSecondsBeforeDestroy);
	destroyFoundation(foundation);
}
