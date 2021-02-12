import { group, sleep } from 'k6';

import {
	Explorer, Grant, Tenant,
	Application, ApplicationDetail,
	GrantType,
	Plan,
} from '../endpoints/endpoints.js'

export let options = {
	vus: 1,
};

let waitSecondsBeforeDestroy = 1;
let waitSecondsWhileCreating = 0.3;
let waitSecondsWhileDestroying = 0.3;

export default function smokeTest() {
	let explorer = null;
	let grant = null;
	let tenant = null;
	let application = null;
	let applicationDetail = null;
	let grantType = null;
	let plan = null;
	group('smoke test: create foundation', function createFoundation() {
		/**
		 * CREATE explorer
		 */
		group('create explorer', function createExplorer() {
			explorer = new Explorer(true);
		});

		/**
		 * CREATE explorer/grant
		 */
		sleep(waitSecondsWhileCreating);
		group('create grant', function createGrant() {
			grant = new Grant(explorer, true);
		});

		/**
		 * CREATE explorer/grant/tenant
		 */
		sleep(waitSecondsWhileCreating);
		group('create tenant', function createTenant() {
			tenant = new Tenant(grant, true);
		});

		/**
		 * CREATE explorer/grant/tenant/application
		 */
		sleep(waitSecondsWhileCreating);
		group('create application', function createApplication() {
			application = new Application(tenant, true);
		});
		sleep(waitSecondsWhileCreating);
		group('create applicationdetail', function createApplicationDetail() {
			applicationDetail = new ApplicationDetail(application, true);
		});

		/**
		 * CREATE explorer/grant/tenant/grant-type
		 */
		sleep(waitSecondsWhileCreating);
		group('create granttype', function createGrantType() {
			grantType = new GrantType(tenant, true);
		});

		/**
		 * CREATE explorer/grant/tenant/plan
		 */
		sleep(waitSecondsWhileCreating);
		group('create plan', function createPlan() {
			plan = new Plan(tenant, true);
		});
	});

	sleep(waitSecondsBeforeDestroy);

	group('smoke test: destroy foundation', function destroyFoundation() {
		/**
		 * DELETE explorer/grant/tenant/plan
		 */
		sleep(waitSecondsWhileDestroying);
		group('delete plan', function destroyPlan() {
			plan.delete();
		});
		
		/**
		 * DELETE explorer/grant/tenant/grant-type
		 */
		sleep(waitSecondsWhileDestroying);
		group('delete grantType', function destroyGrantType() {
			grantType.delete();
		});
		
		/**
		 * DELETE explorer/grant/tenant/application
		 */
		group('delete grant', function destroyApplicationDetail() {
			applicationDetail.delete();
		});
		sleep(waitSecondsWhileDestroying);
		group('delete application', function destroyApplication() {
			application.delete();
		});

		/**
		 * DELETE explorer/grant/tenant
		 */
		sleep(waitSecondsWhileDestroying);
		group('delete tenant', function destroyTenant() {
			tenant.delete();
		});

		/**
		 * DELETE explorer/grant
		 */
		sleep(waitSecondsWhileDestroying);
		group('delete grant', function destroyGrant() {
			grant.delete();
		});

		/**
		 * DELETE explorer
		 */
		sleep(waitSecondsWhileDestroying);
		group('delete explorer', function destroyExplorer() {
			explorer.delete();
		});
	});
}
