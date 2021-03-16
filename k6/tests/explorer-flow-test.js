import { group, sleep } from 'k6';

import {createFoundation, destroyFoundation} from '../utils/foundation.js';
import {apiMakeData, apiCreate, apiDelete, apiMakeMetrics, apiRunWithMetrics} from '../api/api.js';
import { defaultOrEnv, FIELD_VARIABLES_GLOBAL_VALUES, logServer } from '../utils/utils.js';
import { config } from '../utils/config.js';

let waits = {
	create: null,
	delete: null,
};

let metrics = {
	explorer_type: {
		create: apiMakeMetrics("k_explorer_types_create"),
		delete: apiMakeMetrics("k_explorer_types_delete"),
	},
	equipment: {
		create: apiMakeMetrics("k_equipments_create"),
		delete: apiMakeMetrics("k_equipments_delete"),
	},
	explorer: {
		create: apiMakeMetrics("k_explorers_create"),
		delete: apiMakeMetrics("k_explorers_delete"),
	},
};

export function setup() {
	logServer();
	waits.create = defaultOrEnv(0, "K6S_WAIT_CREATE");
	waits.delete = defaultOrEnv(0, "K6S_WAIT_DELETE");

	FIELD_VARIABLES_GLOBAL_VALUES.TAG = defaultOrEnv("", "K6S_TAG");
	let data = {
		foundation: createFoundation(),
		createOnly: defaultOrEnv(false, "K6S_CREATE_ONLY"),
	};

	return data;
}

export default function explorerFlow(data) {
	config.options.traceTimings = defaultOrEnv(false, "K6S_TRACE_TIMINGS", false);
	let flow = {};
	//console.log(`DEBUG: Explorer flow iteration ${__ITER}, VU ${__VU}`);

	group("create things", () => {
		//console.log(`DEBUG: Explorer flow CREATE, iteration ${__ITER}, VU ${__VU}`);
		flow.explorer_type = apiMakeData("explorer_types", data.foundation.provider);
		apiRunWithMetrics(apiCreate, flow.explorer_type, waits.create, metrics.explorer_type.create);

		flow.equipment = apiMakeData("equipments", data.foundation.tenant);
		apiRunWithMetrics(apiCreate, flow.equipment, waits.create, metrics.equipment.create);

		flow.explorer = apiMakeData("explorers", data.foundation.tenant);
		flow.explorer.configObject.type_id = data.foundation.explorer_type.id;
		flow.explorer.configObject.equipment_ids = [];
		flow.explorer.configObject.equipment_ids.push(flow.equipment.id);
		apiRunWithMetrics(apiCreate, flow.explorer, waits.create, metrics.explorer.create);
	});

	if (data.createOnly) {
		//console.log(`DEBUG: VU ${__VU} / iteration ${__ITER} is being told to create only, skipping deletion.`);
	} else {
		group("delete things", () => {
			//console.log(`DEBUG: Explorer flow DELETE, iteration ${__ITER}, VU ${__VU}`);
			apiRunWithMetrics(apiDelete, flow.explorer, waits.delete, metrics.explorer.delete);
			apiRunWithMetrics(apiDelete, flow.equipment, waits.delete, metrics.equipment.delete);
			apiRunWithMetrics(apiDelete, flow.explorer_type, waits.delete, metrics.explorer_type.delete);
		});
	}
}

export function teardown(data) {
	if (data.createOnly) {
		console.log(`DEBUG: We're being told to create only, skipping destroy foundation.`);
	} else {
		destroyFoundation(data.foundation);
	}
}
