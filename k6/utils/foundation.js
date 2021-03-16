import { sleep } from 'k6';

import {apiMakeData, apiCreate, apiDelete, apiGetAll} from '../api/api.js';

let waitSecondsWhileCreating = 0.2;
let waitSecondsWhileDestroying = 0.2;

export function createFoundation() {
	let foundation = {
		organization: null,
		grant: null,
		tenant: null,
		application: null,
		application_detail: null,
		grant_type: null,
	};

	/**
	 * CREATE organization
	 */
	foundation.organization = apiMakeData("organizations");
	apiCreate(foundation.organization);

	/**
	 * CREATE organization/grant
	 */
	sleep(waitSecondsWhileCreating);
	foundation.grant = apiMakeData("grants", foundation.organization);
	apiCreate(foundation.grant);

	/**
	 * CREATE organization/grant/tenant
	 */
	sleep(waitSecondsWhileCreating);
	foundation.tenant = apiMakeData("tenants", foundation.grant);
	apiCreate(foundation.tenant);

	/**
	 * CREATE organization/grant/tenant/application
	 */
	sleep(waitSecondsWhileCreating);
	foundation.application = apiMakeData("applications", foundation.tenant);
	let apps = apiGetAll(foundation.application);
	foundation.application.id = apps[0].id;

	/**
	 * CREATE organization/grant/tenant/application/application-detail
	 */
	sleep(waitSecondsWhileCreating);
	foundation.application_detail = apiMakeData("application_details", foundation.application);
	apiCreate(foundation.application_detail);

	/**
	 * CREATE organization/grant/tenant/grant_type
	 */
	sleep(waitSecondsWhileCreating);
	foundation.grant_type = apiMakeData("grant_types", foundation.tenant);
	apiCreate(foundation.grant_type);

	return foundation;
}

export function destroyFoundation(foundation) {
	/**
	 * DELETE organization/grant/tenant/grant_type
	 */
	sleep(waitSecondsWhileDestroying);
	apiDelete(foundation.grant_type);
	
	/**
	 * DELETE organization/grant/tenant/application/application-detail
	 */
	sleep(waitSecondsWhileDestroying);
	apiDelete(foundation.application_detail);

	/**
	 * DELETE organization/grant/tenant
	 */
	sleep(waitSecondsWhileDestroying);
	apiDelete(foundation.tenant);

	/**
	 * DELETE organization/grant
	 */
	sleep(waitSecondsWhileDestroying);
	apiDelete(foundation.grant);

	/**
	 * DELETE organization
	 */
	sleep(waitSecondsWhileDestroying);
	apiDelete(foundation.organization);
};
