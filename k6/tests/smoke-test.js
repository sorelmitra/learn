import { sleep } from 'k6';

import {createFoundation, destroyFoundation} from '../utils/foundation.js';

let waitSecondsBeforeDestroy = 1;

export default function smokeTest() {
	let foundation = createFoundation();
	sleep(waitSecondsBeforeDestroy);
	destroyFoundation(foundation);
}
