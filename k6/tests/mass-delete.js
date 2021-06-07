import { sleep } from 'k6';

import {apiMakeData, apiGetAll, apiDelete} from '../api/api.js';
import {defaultOrEnv, no, pad} from '../utils/utils.js';

/**
 * Array of things to delete/display.
 * Each thing to delete is represented by its parent followed by a `/`
 * then by the actual thing to delete.  The "root" which does not 
 * have a parent uses `root` as it's formal parent.  
 * The array is iterated in reverse order when building the tree 
 * and in normal order when deleting.
 * 
 * isLeaf must be set to true for items which don't have children, 
 * and to false for items which have children.
 * 
 * skipDelete must be set to true for items which must not be deleted,
 * e.g. those that the API you're calling is deleting automatically.
 * If it's ommitted, it is assumed to be false.
 * 
 * Based on the isLeaf value the code knows when to stop searching
 * for the common ancestor, i.e. don't wait for all children X of
 * parent Y1 to be deleted if we're in parent Y2.  To achieve this,
 * the function findFirstDefinedNotLeafAncestor() walks the array below
 * to first find the definition of X.
 * Then it goes on to find the definition of Y, where Y is the first
 * element that has isLeaf false and that is an ancestor of X.
 * 
 * Further on, once the common defined ancestor that is not a leaf
 * was found, the code in getPredecessorCount() makes sure that
 * when watching for all children in hasPredecessorInstances()
 * we don't wait for instances of X to be deleted from parent Y1
 * if we're in parent Y2.
 */
let orderedItemsRootName = "root";
let orderedItemsToDelete = [
	{item: `tenants/explorers`, isLeaf: true},
	{item: `tenants/explorer_types`, isLeaf: true},
	{item: `tenants/grant_types`, isLeaf: true},
	{item: `applications/application_details`, isLeaf: true},
	{item: `tenants/applications`, isLeaf: false, skipDelete: true},
	{item: `grants/tenants`, isLeaf: false},
	{item: `organizations/grants`, isLeaf: false},
	{item: `${orderedItemsRootName}/organizations`, isLeaf: false},
];


/**
 * SETUP.  Build, log, and return tree of API items.
 */
export function setup() {
	let root = makeRootNode("root");
	let data = {
		root: root,
		countVus: defaultOrEnv(0, "K6_VUS"),
		waitDelete: defaultOrEnv(0, "K6S_WAIT_DELETE"),
		displayOnly: defaultOrEnv(false, "K6S_DISPLAY_ONLY"),
		secondsToWaitForPredecessorDeletion: defaultOrEnv(0, "K6_SECONDS_WAIT_PREVIOUS_DELETE"),
	};
	if (data.countVus == 0) {
		console.log("ERROR: Please specify the exact number of VUs in the 'K6_VUS' variable!");
		return data;
	}
	buildTree(root, null);
	logTree(root);
	return data;
}


/**
 * Actual deletion, done in parallel.
 * Walk the tree and delete items in the order specified.
 * Currently it doesn't wait for all preceding items 
 * to be deleted before deleting the current item.
 */
export default function massDelete(data) {
	if (data.displayOnly) {
		console.log(`VU ${__VU} requested to display only, returning.`);
		return;
	}
	deleteTree(data.root, data.countVus, data.waitDelete, data.secondsToWaitForPredecessorDeletion);
}


/**
 * Build the tree.
 */
function buildTree(node, groupApiData) {
	if (node.isRoot) {
		console.log(`DEBUG: buildTree: ROOT ${node.apiData.name}`);
		if (!buildChildGroupsSubtree(node, groupApiData)) {
			console.log(`WARNING: buildTree: Root has no defined kids, nothing to do`);
		}
		return;
	}

	console.log(`DEBUG: buildTree: API ${groupApiData.path}`);
	let allInstances = apiGetAll(groupApiData);
	if (no(allInstances) || allInstances.length < 1) {
		console.log(`DEBUG: buildTree: ${node.apiName} has no instances`);
		return;
	}

	allInstances.forEach(apiInstance => {
		let instanceNode = createAndFillApiNode(apiInstance, groupApiData);
		node.children.push(instanceNode);
		buildChildGroupsSubtree(instanceNode, instanceNode.apiData);
	});
}


/**
 * Delete the tree:
	- START
	- FOR each item in the array to delete:
		1. Check predecessor in the array:
			- IF it doesn't exist, THEN GO TO to step 2. with success
			- IF it exists, THEN:
				1. A. Walk the tree and make `GetAll` on that predecesor for all instances of its parent.
				2. IF no instances of the predecessor exist, THEN GO TO step 2. with success
				3. A. IF waited for N times and instances of the predecessor still exist, THEN GO TO step 2. with failure
				4. A. Wait T ms.
				5. A. GOTO step 1.A.
		2. IF the predecessor still has instances, STOP
		3. Walk the tree and make `Delete` on all instances of the thing to delete.
	- STOP
 */
function deleteTree(root, countVus, waitDelete, secondsToWaitForPredecessorDeletion) {
	if (__VU > countVus) {
		console.log(`VU ${__VU} > total VUs ${countVus}, doing nothing`);
		return;
	}
	let shouldCheckPredecessorStatus = true;
	let predecessorsDeleted = true;
	walkTree("postorder", 0, 1, root, (data, customData) => {
		if (data.node.isArtificialGroup) {
			data.message = `: FINISHED`;
			logNode(data, customData);
			shouldCheckPredecessorStatus = true;
			predecessorsDeleted = true;
			return;
		}

		if (!isIndexAssignedToCurrentVU(countVus, data.index)) {
			return;
		}

		let childrenStatus = hasChildInstances(data.node, data.hasChildren, secondsToWaitForPredecessorDeletion);
		if (!childrenStatus.areDeleted) {
			data.message = `: DELETE FAILED, has ${childrenStatus.instancesLeft} children after ${childrenStatus.waitTime} seconds!`;
			logNode(data, customData);
			return;
		}

		if (shouldCheckPredecessorStatus) {
			shouldCheckPredecessorStatus = false;
			let predecessorStatus = hasPredecessorInstances(root, data.node, secondsToWaitForPredecessorDeletion);
			data.message = `: Predecessors ${predecessorStatus.predecessorItem}: ${predecessorStatus.instancesLeft} instances after ${predecessorStatus.waitTime} seconds`;
			if (no(predecessorStatus.predecessorItem)) {
				data.message = ": No predecessors";
			}
			logNode(data, customData);
			predecessorsDeleted = predecessorStatus.areDeleted;
		}

		if (!predecessorsDeleted) {
			data.message = `: DELETE FAILED, has predecessors not deleted!`;
			logNode(data, customData);
			return;
		}

		//if (data.index == 1) return;  // Test predecessor check

		if (hasSkipDeleteInDefinition(data.node.apiData.name)) {
			return;
		}
		let deleted = apiDelete(data.node.apiData);
		sleep(waitDelete);
		let deletedStr = deleted ? "OK" : "FAILED";
		data.message = `: DELETED ${deletedStr}`;
		logNode(data, customData);
	});
}


/**
 * Helper Function: Partition to VUs based on index in tree children array.
 */
function isIndexAssignedToCurrentVU(countVus, index) {
	if (countVus < 2) {
		return true;
	}
	return index % countVus == __VU - 1;
}


/**
 * Log the tree.
 */
function logTree(root) {
	console.log(`This is your tree:`);
	console.log(`Each node is displayed with the following info: <+|-> level:`);
	console.log(` <+|-> <level>:<number inside level>(<overall number>) <name> <meaningful info>`);
	walkTree("preorder", 0, 1, root, logNode);
	for (let i = 0; i < 60; i++) {
		console.log(`=================`);
	}
}


/**
 * Helper function: Build subtree for all node's child groups.
 * Called as part of the recursive function buildTree().
 */
function buildChildGroupsSubtree(node, groupApiData) {
	let definedChildren = getDefinedChildren(node);
	if (no(definedChildren)) {
		return false;
	}
	definedChildren.forEach(kidName => buildGroupSubtree(node, kidName, groupApiData));
	return true;
}

/**
 * Helper function: Build a group subtree.
 * Called as part of the recursive function buildTree().
 */
function buildGroupSubtree(node, groupName, groupApiData) {
	let apiData = apiMakeData(groupName, groupApiData);
	let groupNode = makeGroupNode(groupName);
	node.children.push(groupNode);
	console.log(`DEBUG: buildTree: GROUP ${groupNode.apiName}, API ${apiData.name}`);
	buildTree(groupNode, apiData);
}

/**
 * Helper function: Create API node and fill it in 
 * with data based on the current tree state.
 */
function createAndFillApiNode(apiInstance, groupApiData) {
	/**
	 * Should've called cloneAndReplaceVars() here but for some reason
	 * the K6 engine crashes when I do.
	 */
	let instanceApiData = {
		name: groupApiData.name,
		groupApiData: groupApiData.groupApiData,
		configObject: groupApiData.configObject,
		path: groupApiData.path,
		parentApiData: groupApiData.parentApiData,
		id: apiInstance.id,
	};
	let meaningfulName = apiInstance.id;
	if (!no(apiInstance.name)) {
		meaningfulName = apiInstance.name;
	}
	if (!no(apiInstance.key)) {
		meaningfulName = apiInstance.key;
	}
	if (!no(apiInstance.native_id)) {
		meaningfulName = apiInstance.native_id;
	}
	if (!no(apiInstance.login_id)) {
		meaningfulName = apiInstance.login_id;
	}
	if (!no(apiInstance.unique_id)) {
		meaningfulName = apiInstance.unique_id;
	}
	if (!no(apiInstance.handle)) {
		meaningfulName = apiInstance.handle;
	}
	if (!no(apiInstance.address)) {
		meaningfulName = apiInstance.address;
	}
	return makeApiNode(instanceApiData, meaningfulName);
}

/**
 * Helper function: Create root node.
 */
function makeRootNode(name) {
	let node = makeGroupNode(name, true);
	node.apiData.name = name;
	return node;
}

/**
 * Helper function: Create group node.
 */
function makeGroupNode(name, isRoot = false) {
	let node = makeNode(true, isRoot);
	node.apiData = {
		name: `GROUP of ${name}`, 
		id: "",
	};
	node.apiName = name;
	node.meaningfulName = node.apiData.name;
	return node;
}

/**
 * Helper function: Create API node.
 */
function makeApiNode(apiData, meaningfulName) {
	let node = makeNode(false);
	node.apiData = apiData;
	node.meaningfulName = meaningfulName;
	return node;
}

/**
 * Helper function: Create common node.
 * apiData must be set on the node after calling this function.
 */
function makeNode(isArtificialGroup, isRoot = false) {
	return {
		apiData: null,
		children: [],
		isArtificialGroup: isArtificialGroup,
		isRoot: isRoot,
	};
}


/**
 * Helper function: Get defined children for a node based on the API name.
 */
function getDefinedChildren(node) {
	let definedChildren = [];

	orderedItemsToDelete.forEach(itemDefinition => {
		let ancestorArray = splitAncestors(itemDefinition.item);
		let len = getValidatedAncestorsLength(ancestorArray);
		if (no(len)) return null;
		let parent = ancestorArray[len - 2];
		if (node.apiData.name == parent) {
			let child = ancestorArray[len - 1];
			definedChildren.push(child);
		}
	});

	if (definedChildren.length < 1) {
		definedChildren = null;
	}

	return definedChildren;
}


/**
 * Helper function: Get defined predecessor for a node based on the API name.
 */
function getDefinedPredecessor(node) {
	let index = 0;
	let foundIndex = -1;

	orderedItemsToDelete.forEach(itemDefinition => {
		let ancestorArray = splitAncestors(itemDefinition.item);
		let len = getValidatedAncestorsLength(ancestorArray);
		if (no(len)) return null;
		let child = ancestorArray[len - 1];
		if (node.apiData.name == child) {
			foundIndex = index - 1;
			return;
		}
		index++;
	});

	//console.log(`DEBUG: Predecessor index of ${node.apiData.name} is ${foundIndex}`);
	if (foundIndex < 0) {
		return null;
	}

	return orderedItemsToDelete[foundIndex].item;
}


/**
 * Helper function: Split item definition into an array with the ancestors
 * first and then the child.
 */
function splitAncestors(itemDefinition) {
	return itemDefinition.split("/");
}


/**
 * Helper function: Get length of ancestors array while validating it.
 */
function getValidatedAncestorsLength(ancestorArray) {
	let len = ancestorArray.length;
	if (len < 2) {
		console.log(`ERROR: orderedItemsToDelete contains item <${ancestorArray[0]}> which is not of the form <ancestors.../child>`);
		return null;
	}
	return len;
}


/**
 * Helper function: Does node have instances of its defined predecessors?
 */
function hasPredecessorInstances(root, node, secondsToWaitForPredecessorDeletion) {
	let status = {
		areDeleted: true,
		instancesLeft: 0,
		waitCycles: 0,
		sleepTime: 0,
	};

	let definedPredecessorItem = getDefinedPredecessor(node);
	let data = {
		itemDefinition: definedPredecessorItem,
		node: node,
	};
	//console.log(`DEBUG: Defined predecessor of ${node.apiData.name} is ${definedPredecessorItem}`);
	if (!no(definedPredecessorItem)) {
		console.log(`DEBUG: [VU${pad(__VU, 3)}] Waiting for predecessors of ${getNodeNameInContext(node)} to be deleted`);
		status = waitForTreeItemsToDisappear(root, getPredecessorCount, secondsToWaitForPredecessorDeletion, data);
	}

	status.predecessorItem = definedPredecessorItem;
	return status;
}


/**
 * Helper function: Get node name with meaningful context
 */
function getNodeNameInContext(node) {
	if (no(node.apiData.path)) {
		return node.apiData.name;
	}

	return node.apiData.path;
}


/**
 * Helper function: Does node have instances of its defined children?
 */
function hasChildInstances(node, hasChildren, secondsToWaitForPredecessorDeletion) {
	let status = {
		areDeleted: true,
		instancesLeft: 0,
		waitCycles: 0,
		sleepTime: 0,
	};

	if (hasChildren) {
		status = waitForTreeItemsToDisappear(node, getChildrenCount, secondsToWaitForPredecessorDeletion);
	}

	return status;
}


/**
 * Helper function: Waits for tree items to dissappear.
 * Count of items is returned by the callback.
 */
function waitForTreeItemsToDisappear(root, getItemsCount, secondsToWaitForPredecessorDeletion, callbackData = null) {
	let status = {
		areDeleted: false,
		instancesLeft: 0,
		sleepTime: 10,
		waitCycles: null,
	};
	status.waitCycles = Math.ceil(secondsToWaitForPredecessorDeletion / status.sleepTime);

	let currentCycle = 0;
	while (true) {
		let itemsCountData = getItemsCount(root, callbackData);
		status.instancesLeft = itemsCountData.count;
		console.log(`DEBUG: [VU${pad(__VU, 3)}] waitForTreeItemsToDisappear: instances of ${itemsCountData.name}: left ${status.instancesLeft}, cycle ${currentCycle}/${status.waitCycles}, sleep time ${status.sleepTime} seconds`);
		if (status.instancesLeft < 1) {
			status.areDeleted = true;
			break;
		}
		currentCycle++;
		if (currentCycle >= status.waitCycles) {
			break;
		}
		sleep(status.sleepTime);
	}

	status.waitTime = currentCycle * status.sleepTime;
	return status;
}


/**
 * Helper function: Count existing children of node.
 */
function getChildrenCount(node) {
	let instancesLeft = 0;
	let definedChildren = getDefinedChildren(node);
	if (no(definedChildren)) {
		return instancesLeft;
	}
	definedChildren.forEach(childName => {
		if (hasSkipDeleteInDefinition(childName)) {
			return;
		}
		let childApiData = apiMakeData(childName, node.apiData);
		let childInstances = apiGetAll(childApiData);
		instancesLeft += childInstances.length;
	});
	return {
		count: instancesLeft,
		name: `children of ${getNodeNameInContext(node)}`,
	};
}


/**
 * Helper function: Count existing instances of node's predecessor in the
 * array of items to delete.
 */
function getPredecessorCount(root, predecessorCountData) {
	let ancestorArray = splitAncestors(predecessorCountData.itemDefinition);
	let len = getValidatedAncestorsLength(ancestorArray);
	if (no(len)) {
		return {
			count: 1,
			name: `predecessors of ${getNodeNameInContext(predecessorCountData.node)} - WRONG DATA ITEM DEFINITION!`,
		};
	}

	let parent = ancestorArray[len - 2];
	let child = ancestorArray[len - 1];

	let originalAncestorId = getCommonAncestorId(predecessorCountData.node);
	if (no(originalAncestorId)) {
		return {
			count: 0,
			name: `predecessors of ${getNodeNameInContext(predecessorCountData.node)}`,
		};
	}

	let predecessorCount = 0;
	walkTree("preorder", 0, 1, root, (data, customData) => {
		if (no(customData.shouldCount)) {
			customData.shouldCount = false;
		}

		let currentAncestorId = getCommonAncestorId(data.node);
		if (currentAncestorId != originalAncestorId) {
			return;
		}

		if (data.node.apiData.name == parent) {
			customData.shouldCount = true;
			return;
		}

		if (data.node.apiData.name != child) {
			return;
		}

		if (!customData.shouldCount) {
			return;
		}

		if (predecessorCountData.itemDefinition.skipDelete) {
			return;
		}

		customData.shouldCount = false;
		let instances = apiGetAll(data.node.apiData);
		predecessorCount += instances.length;
		//console.log(`DEBUG: Instances of ${predecessorCountData.itemDefinition}: ${instances.length}`);
	});

	//console.log(`DEBUG: Total instances of ${predecessorCountData.itemDefinition}: ${predecessorCount}`);
	return {
		count: predecessorCount,
		name: `predecessors of ${getNodeNameInContext(predecessorCountData.node)}`,
	};
}


/**
 * Helper function: Get common ancestor ID, i.e. the subtree root.
 */
function getCommonAncestorId(node) {
	let commonAncestorName = findFirstDefinedNotLeafAncestor(node);
	if (no(commonAncestorName)) {
		return null;
	}

	let apiData = node.apiData;
	while (!no(apiData)) {
		if (apiData.name == commonAncestorName) {
			break;
		}
		apiData = apiData.parentApiData;
	}
	if (no(apiData)) {
		return null;
	}
	return apiData.id;
}

/**
 * Helper function: Find the first defined ancestor that is not leaf
 * in the ordered items to delete.
 */
function findFirstDefinedNotLeafAncestor(node) {
	let childName = node.apiData.name;
	let parentName = null;
	while (true) {
		let itemDefinition = getItemDefinition(childName);
		if (no(itemDefinition)) break;
		let ancestorArray = splitAncestors(itemDefinition.item);
		let len = getValidatedAncestorsLength(ancestorArray);
		if (no(len)) break;
		parentName = ancestorArray[len - 2];
		let parentItemDefinition = getItemDefinition(parentName);
		if (no(parentItemDefinition)) {
			if (parentName != orderedItemsRootName) {
				console.log(`ERROR: Cannot find item definition for ${parentName}`);
			}
			break;
		}
		if (!parentItemDefinition.isLeaf) {
			break;
		}
		childName = parentName;
	}
	//console.log(`DEBUG: Common ancestor of ${node.apiData.name} is ${parentName}`);
	return parentName;
}

/**
 * Helper function: Whether the given node name has a member named "skipDelete"
 * and that member has the value of true.
 */
function hasSkipDeleteInDefinition(nodeName) {
	let itemDefinition = getItemDefinition(nodeName);
	if (no(itemDefinition)) {
		console.log(`ERROR: Cannot find item definition for ${nodeName}`);
		return false;
	}
	if (no(itemDefinition.skipDelete)) {
		return false;
	}
	return itemDefinition.skipDelete;
}

/**
 * Helper function: Get item definition of given node.
 */
function getItemDefinition(childName) {
	let foundItemDefinition = null;
	//console.log(`DEBUG: Getting item definition for ${childName}`);
	orderedItemsToDelete.forEach(itemDefinition => {
		let ancestorArray = splitAncestors(itemDefinition.item);
		let len = getValidatedAncestorsLength(ancestorArray);
		if (no(len)) return null;
		let child = ancestorArray[len - 1];
		if (childName == child) {
			//console.log(`DEBUG: childName ${childName} == child ${child}`);
			foundItemDefinition = itemDefinition;
		}
	});

	return foundItemDefinition;
}


/**
 * Helper function: Walk the tree and call back using
 * preorder or postorder.
 */
function walkTree(order, level, index, node, callback, customData = null) {
	if (no(customData)) {
		customData = {};
	}
	let data = {
		level: level,
		index: index,
		node: node,
		hasChildren: node.children.length > 0,
		message: "",
	};
	if (order.toLowerCase() == "preorder") {
		callback(data, customData);
	}
	/*
	let s = "";
	node.children.forEach(kid => s = `${s} ${kid.meaningfulName}`);
	console.log(`DEBUG walkTree ${node.apiData.name}:${s}`);
	*/
	for (let i = 0; i < node.children.length; i++) {
		let child = node.children[i];
		walkTree(order, level + 1, i + 1, child, callback, customData);
	}
	if (order.toLowerCase() == "postorder") {
		callback(data, customData);
	}
}

/**
 * Helper function: log a single node.
 */
function logNode(data, customData) {
	if (no(customData.totalNodeCount)) customData.totalNodeCount = 0;

	let indentStep = "  ";
	let indent = "";
	for (let i = 0; i < data.level; i++) {
		indent = `${indent}${indentStep}`;
	}

	let branchOrLeaf = "-";
	if (data.hasChildren) {
		branchOrLeaf = "+";
	}
	
	indent = `${indent} ${branchOrLeaf} `;

	let nodeIndexStr, nodeInfo;
	let vuStr = `VU${pad(__VU, 3)}`;
	if (data.node.isArtificialGroup) {
		if (__VU > 0) {
			nodeIndexStr = `[${vuStr}] `;
		} else {
			nodeIndexStr = " ";
		}
		nodeInfo = "";
	} else {
		customData.totalNodeCount++;
		if (__VU > 0) {
			nodeIndexStr = `${pad(data.index, 4)}[${pad(customData.totalNodeCount, 4)}][${vuStr}] `;
		} else {
			nodeIndexStr = `${pad(data.index, 4)}[${pad(customData.totalNodeCount, 6)}] `;
		}
		nodeInfo = ` ${data.node.meaningfulName} - ${data.node.apiData.id}`;
	}

	console.log(`${indent}${pad(data.level, 2)}:${nodeIndexStr}${data.node.apiData.name}${nodeInfo}${data.message}`);
}

