import { defaultOrEnv, no } from "./utils.js";

export let config = {
	options: {
		traceTimings: false,
	},
	admin: {
		server: defaultOrEnv("1.2.3.4", "K6S_SERVER", false),
		port: defaultOrEnv(null, "K6S_PORT", false),
		path: "my-sample-service/api",
		version: "v1",
		url: function url() {
			let serverDesignation = `${this.server}:${this.port}`;
			if (no(this.port)) {
				serverDesignation = this.server;
			}
			return `http://${serverDesignation}/${this.path}/${this.version}`;
		},

		/**
		 * The names of the objects below match the Sample API names.
		 * That's why we're not using CamelCase.
		 */

		/**
		 * organization
		 */
		organizations: {
			path: "organizations",
			description: "Sample Organization ${TAG}${VU}_${ITER}",
			name: "organization ${VU}_${ITER}",
		},

		/**
		 * organization/grant
		 */
		grants: {
			path: "grants",
			description: "Sample Grant ${VU}_${ITER}",
			name: "grant ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant
		 */
		tenants: {
			path: "tenants",
			description: "Sample Tenant ${VU}_${ITER}",
			external_tenant_id: "tenant external id ${VU}_${ITER}",
			name: "tenant ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/application
		 */
		applications: {
			path: "applications",
			description: "Sample Application ${VU}_${ITER}",
			name: "application ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/application/application_details
		 */
		application_details: {
			path: "application-details",
			description: "Sample Application Detail ${VU}_${ITER}",
			group: "application detail ${VU}_${ITER}",
			key: "detail key ${VU}_${ITER}",
			value: "detail value ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/grant-type
		 */
		grant_types: {
			path: "grant-types",
			description: "Sample Grant Type ${VU}_${ITER}",
			name: "grant type ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/explorer-type
		 */
		explorer_types: {
			path: "explorer-types",
			description: "Sample Explorer Type ${VU}_${ITER}",
			type: "explorer type ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/equipments
		 */
		equipments: {
			path: "equipments",
			description: "Sample Equipment ${VU}_${ITER}",
			type: "equipment ${VU}_${ITER}",
		},

		/**
		 * organization/grant/tenant/explorer
		 */
		explorers: {
			path: "explorers",
			description: "Sample Explorer ${VU}_${ITER}",
			name: "explorer ${VU}_${ITER}",
			type_id: null,   // Set in the code
			equipment_ids: null,   // Set in the code
		},

		/**
		 * organization/grant/tenant/plan
		 */
		plans: {
			path: "plans",
			description: "Sample Plan ${VU}_${ITER}",
			target: "Travel target ${VU}_${ITER}",
			charts: [
				"zone_${VU}_${ITER}.svg",
				"detailed_${VU}_${ITER}.svg"
			],
			name: "travel plan ${VU}_${ITER}",
			due: "2021-02-15",
			tags: [
				"arctic", "cold",
			],
			author: "this guy",
		},
	},
};

