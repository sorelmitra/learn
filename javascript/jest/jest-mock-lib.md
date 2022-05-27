#  Mock Library or Node Module with Jest

Last updated: 2022-05-26.

I struggled a lot to get this working, so here's the recipe:

Say you have a DB code that imports `aws-sdk/clients/dynamodb`.  To mock that, say something like:

	jest.mock('aws-sdk/clients/dynamodb', () => {
		return {
			// ...
		}
	}

The key is to mock **precisely** what your code is importing!

See excerpt from a working example in this folder.
