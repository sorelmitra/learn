import { check } from 'k6';
import { config } from './config.js';

export let checks = {

	status: function checkStatus(name, response, expectedStatus) {
		let checks = {};
		checks[`${name} / response status is ${expectedStatus}`] = 
			r => expectedStatus == r.status;
		check(response, checks);
	},

	value: function checkValue(name, fieldName, expected, toCheck) {
		let checks = {};
		if (no(toCheck) || toCheck == "") {
			if (no(expected) || expected == "") {
				toCheck = "";
				expected = "";
			}
		}
		checks[`${name} / value of field ${fieldName}`] = 
			value => {
				if (expected == value) return true;
				console.log(`WARNING: Check for field ${name}/${__ITER}_${__VU}/'${fieldName}' failed:\nExpected <${expected}>\n     Got <${value}>\nEnd of WARNING`);
				return false;
			};
		check(toCheck, checks);
	},
};

export let defaultParams = {
	headers: {
		'Content-Type': 'application/json',
	},
};

function isObject(value) {
	return typeof value === 'object' && value !== null;
}

export function no(value) {
	return value == null || value == undefined;
}

export const FIELD_VARIABLES = {
	VIRTUAL_USER: "${VU}",
	ITERATION: "${ITER}",
	TAG: "${TAG}",
};

export const FIELD_VARIABLES_GLOBAL_VALUES = {
	TAG: null,
};

export function insertTagCategory(category, tag) {
	let newTag = tag.replace('_', `_${category}_`);
	return newTag;
}

/**
 * At the time of writing this code, K6 didn't have a replaceAll() on String.prototype...
 */
function replaceAllInString(str, toReplace, replacement) {
	let newStr = str;
	if (typeof newStr !== 'string') {
		return newStr;
	}
	while (newStr.includes(toReplace)) {
		newStr = newStr.replace(toReplace, replacement);
	}
	return newStr;
}

function replaceVariables(str, variableReplacements) {
	let newStr = str;
	for (let fieldVariable in FIELD_VARIABLES) {
		let variableName = FIELD_VARIABLES[fieldVariable];
		let replacementValue = variableReplacements[variableName];
		if (!no(replacementValue)) {
			//replacementValue = addRandomness(variableName, replacementValue);
			newStr = replaceAllInString(newStr, variableName, replacementValue);
		}
	};
	return newStr;
}

function addPseudoRandom(variableName, value) {
	if (variableName != FIELD_VARIABLES.ITERATION) {
		return value;
	}
	let epochMs = Date.now();
	let epochStr = epochMs.toString();
	let pseudoRandom = epochStr.slice(-3);
	value = `${value}_${pseudoRandom}`;
	return value;
}

export function createUUID(){
    var dt = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
    });
    return uuid;
}

function addRandomness(variableName, value) {
	if (variableName != FIELD_VARIABLES.ITERATION) {
		return value;
	}
	value = `${value}_${createUUID()}`;
	return value;
}

export function cloneAndReplaceVars(thing, variableReplacements, keysExcluded = []) {
	if (Array.isArray(thing)) {
		let newArray = [];
		thing.forEach(item => {
			newArray.push(cloneAndReplaceVars(item, variableReplacements, keysExcluded));
		});
		return newArray;
	}

	if (isObject(thing)) {
		let newObject = {};
		for (let key in thing) {
			if (keysExcluded.includes(key)) {
				continue;
			}
			let value = thing[key];
			newObject[key] = cloneAndReplaceVars(value, variableReplacements, keysExcluded);
			//console.log(`cloneAndReplaceVars: key <${key}>, new value ${newObject[key]}`);
		}
		return newObject;
	}
	
	let newValue = replaceVariables(thing, variableReplacements);
	return newValue;
}

export function checkValues(name, fieldName, expectedThing, thingToCheck) {
	if (Array.isArray(expectedThing)) {
		if (!Array.isArray(thingToCheck)) {
			checks.value(fieldName, "of type 'Array'", null);
			return;
		}
		for (let i = 0; i < expectedThing.length; i++) {
			let expectedItem = expectedThing[i];
			let itemToCheck = thingToCheck[i];
			checkValues(name, `${fieldName}[${i}]`, expectedItem, itemToCheck);
		}
		return;
	}

	if (isObject(expectedThing)) {
		if (!isObject(thingToCheck)) {
			checks.value(fieldName, "of type 'object'", null);
			return;
		}
		for (let key in expectedThing) {
			let expectedValue = expectedThing[key];
			let valueToCheck = thingToCheck[key];
			checkValues(name, `${key}`, expectedValue, valueToCheck);
		}
		return;
	}

	checks.value(name, fieldName, expectedThing, thingToCheck);
}

export function defaultOrEnv(defaultValue, envName, trace = true) {
	let value = __ENV[envName];
	let isDefault = false;
	if (no(value)) {
		value = defaultValue;
		isDefault = true;
	}
	value = convertBooleanString(value);
	if (trace) {
		console.log(`${envName}: ${value}, is default: ${isDefault}, type ${typeof value}`);
	}
	return value;
}

export function pad(number, length, prefix = '0') {
	return number.toString().padStart(length, prefix);
}

function convertBooleanString(value) {
	if (typeof value != "string") {
		return value;
	}

	switch(value.toLowerCase()) {
	case "false":
	case "no":
		return false;
	case "true":
	case "yes":
		return true;
	}

	return value;
}

export function logServer() {
	console.log(`Server ${config.admin.server}, port ${config.admin.port}`);
}

