import { sleep } from "k6";
import { createUUID } from "../utils/utils.js";

export default function uuidTest() {
	let uuids = [];
	let count = 200000;
	let milestone = 1000;
	let foundDuplicate = false;
	let sleepTime = 1;
	console.log(`CREATING ${count} UUIDs and checking for uniqueness...`);
	for (let i = 0; i < count; i++) {
		let uuid = createUUID();
		let index = uuids.indexOf(uuid);
		if (index != -1) {
			foundDuplicate = true;
			break;
		}
		uuids.push(uuid);
		if (i % milestone == 0) {
			console.log(`Milestone: ${i} UUIDs generated so far, sleeping ${sleepTime}s...`);
			sleep(sleepTime);
		}
	}
	if (foundDuplicate) {
		console.log(`GOTCHA: UUID ${uuid} already exists in the array at index ${index}!`);
	} else {
		console.log(`SUCCESS: Finished creating ${count} UUIDs and found no duplicate.`);
	}
}
