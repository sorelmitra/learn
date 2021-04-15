# Overview

This tool shows K6 in action with some complex code and configuration.  It builds upon a hypothetical API that refers to exploring the world.  It contains code to create stuff in that API, as well as a mass deletion code that enumerates API items in the given order and deletes them all.

A "flow" in K6 terminology is a sequence of logical actions on a given service.

# Usage

To create 1000 flows of a explorer in 24 parallel executions, you would run this:

```shell
K6_VUS=24 K6_ITERATIONS=1000 K6S_CREATE_ONLY=yes K6S_TRACE_TIMINGS=yes K6S_SERVER=<IP> K6S_PORT=<Port> k6 run --include-system-env-vars tests/explorer-flow-test.js.
```

To delete everything that's been created, you would run this:

```shell
K6_VUS=24 K6_ITERATIONS=24 K6_SECONDS_WAIT_PREVIOUS_DELETE=60 K6S_DISPLAY_ONLY=no K6S_SERVER=<IP> K6S_PORT=<Port> k6 run --include-system-env-vars tests/mass-delete.js
```

NOTE: There are still issues with the number of VUs in mass-delete.  If you notice deletion isn't triggered, adjust the number of VUs up of down.


The environment variables are used to pass in information both to K6 itself and to the scripts we wrote:

- `K6_VUS`: K6 variable, specifies the number of [VUs](https://k6.io/docs/getting-started/running-k6) that K6 should spawn.  Also used by the mass deletion script to split the work between available instances.
- `K6_ITERATIONS`: K6 variable, specifies the number of [Iterations](https://k6.io/docs/using-k6/execution-context-variables), which essentially is how many executions of the test code should K6 do.
- `K6_DURATION`: K6 variable, if used in conjunction with `K6_ITERATIONS`, then it specifies the maximum duration before the script is killed.
- `K6S_SERVER`: Script variable, specifies the server to connect to.
- `K6S_PORT`: Script variable, specifies the port to use when connecting to the server.
- `K6S_TRACE_TIMINGS`: Script variable, boolean true/false or yes/no.  If true, individual timings are being traced in the logs.  Helpful when debugging stuff.
- `K6S_CREATE_ONLY`: Script variable, boolean.  If true, the script that recognizes it only does the creation part.  Otherwise, it deletes what it has created as soon as creation was finished.
- `K6S_DISPLAY_ONLY`: Script variable, applicable to mass-delete.  If true, it will only display the tree but not delete anything.

# Code Structure

## Introduction

At its very basic, K6 proposes a very simple code organization.  You have a function that's being called from parallel instances of the K6 engine, for the given number of iteration and over the maximum duration specified.  That function is called repeatedly each time having different values of 2 variables: `__VU` specifying the current Virtual User, and `__ITER` specifying the iteration number inside that VU.  From these two your function can then construct a unique ID to pass in the payload it needs to generate.  That's pretty much it, but it actually offers pretty powerful parallel programming framework that draws its power precisely from its simplicity: 

- There's no async programming.  K6 handles the concurrency and parallelism and all code you write is synchronous.
- There's no multi-threading synchronization.  K6 calls you in parallel but apart from some data you can return from a setup function there's really no much shared data and not much you could (or normally want to!) synchronize.

The mass-deletion tool makes use of this simple parallel programming model to delete an entire tree in parallel.

## Script Organization

In order to simplify test script creation the scripts have been organized into a `utils` folder which offers some helper tools and configuration, an `api` folder which contains general code for calling a Web API, and `tests` folder which contains the actual tests.

## API and Utils

The API code is organized as follows:

- `api.js` contains common code for calling a Web API, with some particular changes for this particular API we're calling.
- `config.js` has the basic payload template for each API endpoint, plus some general configuration such as server and port.
- `utils.js` offers some helper tools for creating the payload, for doing K6 checks and some other helper tools.
- `foundation.js` contains code to create and destroy the foundation for any other items in the API, which in this case is organized around organizations, grants and tenants.

## Tests

A simple test like `smoke-test.js` only calls to the foundation.

The more elaborate `explorer-flow-test.js` creates a flow of explorers and the data that's required for them.

`uuid-test.js` just tests generating UUID and is not particularly useful otherwise.

## Mass Delete

Mass deletion is an example of a parallel programming that can be achieved within K6.  It does the following:

- The setup code first calls the API and obtains a tree of API resources, based on the known items declared in `orderedItemsToDelete` variable.  This tree is passed to each instance of the `massDelete` function, which does all the work.

- The `massDelete` function walks the tree in post-order and when it reaches a group of items to delete, it uses `__VU`, `K6S_VUS`, the count of items to delete in the current group, and the current item index to decide whether the current item should be deleted.  Essentially the current item is deleted if `index % K6S_VUS == __VU - 1`, which basically splits the entire group of items to delete in disjunct sets assigned to each of the available VUs based on the VU number.

- The other interesting feature is waiting for an entire group of resources to be deleted before we start deleting a new group.  This ordering is defined in the same `orderedItemsToDelete` variable as above, where the order of deletion is defined by walking the array from beginning to end.  So once requests to delete have been launched for the entire group and the code moves to a new group, a Get ALL on the previous group is being issued periodically until it returns an empty set or a timeout expires.  When the empty set is returned, deletion of the new group proceeds.  If the timeout expires the entire process is canceled.

### Known Issues

There are some bugs when deleting a number of resources that's smaller than the number of VUs.  In some cases, deletion might not get triggered for certain items.  In this case, just lower the number of VUs and rerun the tools.

Seldom I noticed that even with a large number of resources some of them might not get deleted, in which case the usual workaround is to rerun the tool with the same parameters.

The above issues are not blocking and while annoying they have easy workarounds.

### Example of how Mass Deletion Works

So assuming the tree of resources looks like this:

- `tenants`
	- `explorers`
		- `explorer1`
		- `explorer2`
		- `explorer3`
		- `explorer4`
	- `explorer_types`
		- `explorer_type1`
		- `explorer_type2`
		- `explorer_type3`
	- `grant_types`
		- `grant_type1`
		- `grant_type2`
		- `grant_type3`
		- `grant_type4`
		- `grant_type5`

Assume that `orderedItemsToDelete` contains this:

```javascript
[
	`tenants/explorers`,
	`tenants/explorer_types`,
	`tenants/grant_types`,
	`root/tenants`,
]
```

Also assume we start 3 VUs.  Then the `explorers` subtree would be split among VUs like this:

- `explorers`
	- `explorer1`: VU 1
	- `explorer4`:: VU 1
	- `explorer2`: VU 2
	- `explorer3`: VU 3

Once delete requests have been issued for the entire `explorers` group, before moving on to `explorer_types` group, a Get ALL requests will be issued for `explorers` and if it returns an empty set, then it will proceed to deleting `explorer_types`.  Otherwise it will start a timer to wait for the actual deletion to happen but will not reissue the deletion requests.
