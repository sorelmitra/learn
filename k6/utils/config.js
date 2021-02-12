
export let lab = {
	ip: "10.1.1.1"
};

export let config = {
	admin: {
		endpointIp: lab.ip,
		port: 8675,
		path: "my-sample-service/api",
		version: "v5",
		url: function url() {
			return `http://${this.endpointIp}:${this.port}/${this.path}/${this.version}`;
		},

		/**
		 * The names of the objects below match the Sample API names.
		 * That's why we're not using CamelCase.
		 */

		/**
		 * explorer
		 */
		explorers: {
			path: "explorers",
			description: "Sample Explorer ${VU}",
			name: "explorer ${VU}",
		},

		/**
		 * explorer/grant
		 */
		grants: {
			path: "grants",
			description: "Sample Grant ${VU}",
			name: "grant ${VU}",
		},

		/**
		 * explorer/grant/tenant
		 */
		tenants: {
			path: "tenants",
			description: "Sample Tenant ${VU}",
			external_tenant_id: "tenant external id ${VU}",
			name: "tenant ${VU}",
		},

		/**
		 * explorer/grant/tenant/application
		 */
		applications: {
			path: "applications",
			description: "Sample Application ${VU}",
			name: "application ${VU}",
		},

		/**
		 * explorer/grant/tenant/application/grant
		 */
		application_details: {
			path: "application-details",
			description: "Sample Application Detail ${VU}",
			group: "application detail ${VU}",
			key: "detail key ${VU}",
			value: "detail value ${VU}",
		},

		/**
		 * explorer/grant/tenant/grant-type
		 */
		grant_types: {
			path: "grant-types",
			description: "Sample Grant Type ${VU}",
			name: "grant type ${VU}",
		},

		/**
		 * explorer/grant/tenant/plan
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

