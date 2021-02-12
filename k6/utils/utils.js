import { check } from 'k6';

export let checks = {

	statuses: function checkStatuses(response, expectedStatuses) {
		let checks = {};
		expectedStatuses.forEach(expectedStatus => {
			checks[`response status is ${expectedStatus}`] = 
				r => expectedStatus == r.status;
		});
		check(response, checks);
	},

	value: function checkValue(fieldName, expected, toCheck) {
		let checks = {};
		checks[`value of field <${fieldName}> is <${expected}>`] = 
			value => expected == value;
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

export const FIELD_VARIABLES = {
	VIRTUAL_USER: "${VU}",
};

/**
 * At the time of writing this code, K6 didn't have a replaceAll() on String.prototype...
 */
function replaceAllInString(str, toReplace, replacement) {
	let newStr = str;
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
		if (replacementValue) {
			newStr = replaceAllInString(str, variableName, replacementValue);
		}
	};
	return newStr;
}

export function cloneAndAppend(thing, variableReplacements, keysExcluded = []) {
	if (Array.isArray(thing)) {
		let newArray = [];
		thing.forEach(item => {
			newArray.push(cloneAndAppend(item, variableReplacements, keysExcluded));
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
			newObject[key] = cloneAndAppend(value, variableReplacements, keysExcluded);
			//console.log(`cloneAndAppend: key <${key}>, new value ${newObject[key]}`);
		}
		return newObject;
	}
	
	let newValue = replaceVariables(thing, variableReplacements);
	return newValue;
}

export function checkValues(fieldName, expectedThing, thingToCheck) {
	if (Array.isArray(expectedThing)) {
		if (!Array.isArray(thingToCheck)) {
			checks.value(fieldName, "of type 'Array'", null);
			return;
		}
		for (let i = 0; i < expectedThing.length; i++) {
			let expectedItem = expectedThing[i];
			let itemToCheck = thingToCheck[i];
			checkValues(`${fieldName}[${i}]`, expectedItem, itemToCheck);
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
			checkValues(`${key}`, expectedValue, valueToCheck);
		}
		return;
	}

	checks.value(fieldName, expectedThing, thingToCheck);
}
