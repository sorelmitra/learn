
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
			description: "Sample Organization ${VU}_${__ITER}",
			name: "organization ${VU}_${__ITER}",
		},

		/**
		 * organization/grant
		 */
		grants: {
			path: "grants",
			description: "Sample Grant ${VU}_${__ITER}",
			name: "grant ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant
		 */
		tenants: {
			path: "tenants",
			description: "Sample Tenant ${VU}_${__ITER}",
			external_tenant_id: "tenant external id ${VU}_${__ITER}",
			name: "tenant ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/application
		 */
		applications: {
			path: "applications",
			description: "Sample Application ${VU}_${__ITER}",
			name: "application ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/application/application_details
		 */
		application_details: {
			path: "application-details",
			description: "Sample Application Detail ${VU}_${__ITER}",
			group: "application detail ${VU}_${__ITER}",
			key: "detail key ${VU}_${__ITER}",
			value: "detail value ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/grant-type
		 */
		grant_types: {
			path: "grant-types",
			description: "Sample Grant Type ${VU}_${__ITER}",
			name: "grant type ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/explorer-type
		 */
		explorer_types: {
			path: "explorer-types",
			description: "Sample Explorer Type ${VU}_${__ITER}",
			type: "explorer type ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/equipments
		 */
		equipments: {
			path: "equipments",
			description: "Sample Equipment ${VU}_${__ITER}",
			type: "equipment ${VU}_${__ITER}",
		},

		/**
		 * organization/grant/tenant/explorer
		 */
		explorers: {
			path: "explorers",
			description: "Sample Explorer ${VU}_${__ITER}",
			name: "explorer ${VU}_${__ITER}",
			type_id: null,   // Set in the code
			equipment_ids: null,   // Set in the code
		},

		/**
		 * organization/grant/tenant/plan
		 */
		plans: {
			path: "plans",
			description: "Sample Plan ${VU}_${__ITER}",
			target: "Travel target ${VU}_${__ITER}",
			charts: [
				"zone_${VU}_${__ITER}.svg",
				"detailed_${VU}_${__ITER}.svg"
			],
			name: "travel plan ${VU}_${__ITER}",
			due: "2021-02-15",
			tags: [
				"arctic", "cold",
			],
			author: "this guy",
		},
	},
};

