
export let lab = {
	ip: "10.1.1.1"
};

export let config = {
	admin: {
		server: defaultOrEnv("1.2.3.4", "K6S_SERVER"),
		port: defaultOrEnv(null, "K6S_PORT"),
		path: "my-sample-service/api",
		version: "v5",
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
			description: "Sample Organization ${VU}",
			name: "organization ${VU}",
		},

		/**
		 * organization/grant
		 */
		grants: {
			path: "grants",
			description: "Sample Grant ${VU}",
			name: "grant ${VU}",
		},

		/**
		 * organization/grant/tenant
		 */
		tenants: {
			path: "tenants",
			description: "Sample Tenant ${VU}",
			external_tenant_id: "tenant external id ${VU}",
			name: "tenant ${VU}",
		},

		/**
		 * organization/grant/tenant/application
		 */
		applications: {
			path: "applications",
			description: "Sample Application ${VU}",
			name: "application ${VU}",
		},

		/**
		 * organization/grant/tenant/application/application_details
		 */
		application_details: {
			path: "application-details",
			description: "Sample Application Detail ${VU}",
			group: "application detail ${VU}",
			key: "detail key ${VU}",
			value: "detail value ${VU}",
		},

		/**
		 * organization/grant/tenant/grant-type
		 */
		grant_types: {
			path: "grant-types",
			description: "Sample Grant Type ${VU}",
			name: "grant type ${VU}",
		},

		/**
		 * organization/grant/tenant/explorer-type
		 */
		explorer_types: {
			path: "explorer-types",
			description: "Sample Explorer Type ${VU}",
			type: "explorer type ${VU}",
		},

		/**
		 * organization/grant/tenant/equipments
		 */
		equipments: {
			path: "equipments",
			description: "Sample Equipment ${VU}",
			type: "equipment ${VU}",
		},

		/**
		 * organization/grant/tenant/explorer
		 */
		explorers: {
			path: "explorers",
			description: "Sample Explorer ${VU}",
			name: "explorer ${VU}",
			type_id: null,   // Set in the code
			equipment_ids: null,   // Set in the code
		},

		/**
		 * organization/grant/tenant/plan
		 */
		plans: {
			path: "plans",
			description: "Sample Plan ${VU}",
			target: "Travel target ${VU}",
			charts: [
				"zone_${VU}.svg",
				"detailed_${VU}.svg"
			],
			name: "travel plan ${VU}",
			due: "2021-02-15",
			tags: [
				"arctic", "cold",
			],
			author: "this guy",
		},
	},
};

