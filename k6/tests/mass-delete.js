import { group, sleep } from 'k6';

import {Explorer, Grant, Tenant, Application, ApplicationDetail, GrantType} from '../endpoints/endpoints.js'

export let options = {
	vus: 1,
};

let waitSecondsWhileCreating = 0.3;
let waitSecondsWhileDestroying = 0.3;

let parent = new Explorer();
let childArrayClassNames = [[Grant], [Tenant], [Application, GrantType], [ApplicationDetail]];
let childIndex = null;

export default function massDelete() {
	group('mass delete: delete all known things', function deleteAllKnownThings() {
		childIndex = -1;
		deleteEntity(parent);
	});
}

function deleteEntity(parent) {
	let allInstances = parent.getAll();
	if (!allInstances) {
		console.log(`NO     <${parent.path}>`)
		return;
	}

	allInstances.forEach(parentInstance => {
		// Recursively delete children first
		childIndex++;
		let childClassNames = childArrayClassNames[childIndex];
		parent.id = parentInstance.id;
		console.log(`ENTER  <${parent.path}>`);
		if (childClassNames) {
			childClassNames.forEach(childClassName => {
				let child = new childClassName(parent);
				deleteEntity(child);
			});
		}

		// Delete parent instances
		parent.delete(true);
		sleep(waitSecondsWhileDestroying);
		console.log(`LEAVE  <${parent.path}>`)
	});
}
