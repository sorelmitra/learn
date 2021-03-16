import { sleep } from 'k6';
import http from 'k6/http';
import { Counter, Trend } from 'k6/metrics';

import {checks, defaultParams, cloneAndReplaceVars, FIELD_VARIABLES, FIELD_VARIABLES_GLOBAL_VALUES, checkValues, no, insertTagCategory, pad} from '../utils/utils.js';
import {config} from '../utils/config.js';

export function apiMakeData(name, parentApiData = null) {
	let apiData = {};
	apiData.name = name;
	apiData.parentApiData = parentApiData;
	apiData.configObject = config.admin[name];
	if (no(parentApiData)) {
		apiData.path = `${apiData.configObject.path}`;
	} else {
		apiData.path = `${parentApiData.path}/${parentApiData.id}/${apiData.configObject.path}`;
	}
	return apiData;
}

export function apiCreate(apiData) {
	let url = `${config.admin.url()}/${apiData.path}`;
	let payload = createPayload(apiData);
	let payloadJson = JSON.stringify(payload);
	let r = http.post(url, payloadJson, defaultParams);
	apiData.res = r;
	let expectedStatus = 201;
	checks.status(apiData.name, r, expectedStatus);
	if (r.status != expectedStatus) {
		console.log(`WARNING: Status for ${apiData.name} is ${r.status}; PAYLOAD: ${payloadJson}; RESPONSE: ${r.body}`);
	} else {
		//console.log(`DEBUG: PAYLOAD for ${apiData.name}: ${payloadJson}; RESPONSE: ${r.body}`);
	}
	try {
		let d = JSON.parse(r.body);
		checkValues(apiData.name, "unused", payload, d);
		if (no(d)) {
			d = {};
		}
		apiData.id = d.id;
	} catch (e) {
		console.log(`ERROR posting to ${url}: <${e}>, response: <${r.body}>`);
	}
}

export function apiGetAll(apiData) {
	let url = `${config.admin.url()}/${apiData.path}`;
	let r = http.get(url);
	//console.log(`DEBUG: Get all for ${apiData.name}, url: <${url}>, response: ${r.body}`);
	apiData.res = r;
	let d = {};
	checks.status(apiData.name, r, 200);
	try {
		d = JSON.parse(r.body);
	} catch (e) {
	}
	if (no(d)) {
		d = {};
	}
	if (no(d[apiData.name])) {
		d[apiData.name] = [];
	}
	return d[apiData.name];
}

export function apiDelete(apiData, logDeleted = false) {
	let success = deleteById(apiData);
	if (logDeleted) {
		console.log(`DELETE <${apiData.path}/${apiData.id}>`);
	}
	return success;
}

export function apiMakeMetrics(metricsName) {
	let trendTag = insertTagCategory("per_request", metricsName);
	let counterTag = insertTagCategory("total_time", metricsName);
	let metrics = {
		name: metricsName,
		trend: new Trend(trendTag),
		counter: new Counter(counterTag, true),
	};
	return metrics;
}

export function apiRunWithMetrics(apiCallback, apiData, sleepTime, metrics) {
	defaultParams.tags = {
		load_test_tag: metrics.name,
	};
	apiCallback(apiData);
	sleep(sleepTime);
	if (!no(metrics.trend)) {
		metrics.trend.add(apiData.res.timings.duration, defaultParams.tags);
	}
	if (!no(metrics.counter)) {
		metrics.counter.add(apiData.res.timings.duration, defaultParams.tags);
	}
	if (config.options.traceTimings) {
		console.log(`TRACE: ${apiCallback.name} on ${apiData.name} ${__VU}_${__ITER} took ${pad(apiData.res.timings.duration.toFixed(2))}ms`);
	}
}

function deleteById(apiData) {
	let url = `${config.admin.url()}/${apiData.path}/${apiData.id}`;
	let r = http.del(url);
	let expectedStatus = 204;
	apiData.res = r;
	checks.status(apiData.name, r, expectedStatus);
	if (r.status != expectedStatus) {
		console.log(`WARNING: DELETION FAILED: ${r.body}`);
		return false;
	}
	return true;
}

function createPayload(apiData) {
	let replacements = {};
	replacements[FIELD_VARIABLES.VIRTUAL_USER] = __VU;
	replacements[FIELD_VARIABLES.ITERATION] = __ITER;
	if (!no(FIELD_VARIABLES_GLOBAL_VALUES.TAG)) {
		replacements[FIELD_VARIABLES.TAG] = FIELD_VARIABLES_GLOBAL_VALUES.TAG;
	}
	return cloneAndReplaceVars(apiData.configObject, replacements, ["path"]);
}

