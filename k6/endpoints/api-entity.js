import http from 'k6/http';

import {config} from '../utils/config.js';
import {checks, defaultParams, cloneAndAppend, FIELD_VARIABLES, checkValues} from '../utils/utils.js';

export class ApiEntity {
	constructor() {
		this.entityName = null;
		this.id = null;
		this.configObject = null;
		this.path = null;
		this.entity = null;
	}

	getAll() {
		let url = `${config.admin.url()}/${this.path}`;
		let r = http.get(url);
		let d = {};
		d[this.entityName] = [];
		checks.statuses(r, [200]);
		try {
			d = JSON.parse(r.body);
		} catch (e) {
		}
		return d[this.entityName];
	}

	create() {
		let url = `${config.admin.url()}/${this.path}`;
		let payload = this.createPayload();
		let r = http.post(url, payload, defaultParams);
		checks.statuses(r, [201]);
		try {
			let d = JSON.parse(r.body);
			checkValues("created entity", this.entity, d);
			this.id = d.id;
		} catch (e) {
			console.log(`ERROR posting to ${url}: <${e}>, response: <${r.body}>`);
		}
	}

	delete(logDeleted = false) {
		this.deleteById(this.id);
		if (logDeleted) {
			console.log(`DELETE <${this.path}/${this.id}>`);
		}
	}

	deleteById(id) {
		let url = `${config.admin.url()}/${this.path}/${id}`;
		let r = http.del(url);
		checks.statuses(r, [204]);
	}

	createPayload() {
		let replacements = {};
		replacements[FIELD_VARIABLES.VIRTUAL_USER] = __VU;
		this.entity = cloneAndAppend(this.configObject, replacements, ["path"]);
		let payload = JSON.stringify(this.entity);
		//console.log(`Payload: <${payload}>`);
		return payload;
	}
}

