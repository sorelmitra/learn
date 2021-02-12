
import {config} from '../utils/config.js';
import {ApiEntity} from './api-entity.js';

export class Explorer extends ApiEntity {
	constructor(createResource = false) {
		super();
		this.entityName = "explorers";
		this.configObject = config.admin.explorers;
		this.path = `${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class Grant extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "grants";
		this.configObject = config.admin.grants;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class Tenant extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "tenants";
		this.configObject = config.admin.tenants;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class Application extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "applications";
		this.configObject = config.admin.applications;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class ApplicationDetail extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "application_details";
		this.configObject = config.admin.application_details;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class GrantType extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "grant_types";
		this.configObject = config.admin.grant_types;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}

export class Plan extends ApiEntity {
	constructor(parentEntity, createResource = false) {
		super();
		this.parentEntity = parentEntity;
		this.entityName = "plans";
		this.configObject = config.admin.plans;
		this.path = `${parentEntity.path}/${parentEntity.id}/${config.admin[this.entityName].path}`;
		if (createResource) this.create();
	}
}
