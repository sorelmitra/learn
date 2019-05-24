from colorama import Fore, Style

def identityCompare(a, b):
	return a == b

def computeStringDifferences(x, y):
	diffX = ''
	diffY = ''
	redified = False
	for i in range(0, len(x)):
		c1 = x[i]
		c2 = ''
		if i < len(y):
			c2 = y[i]
		if c1 != c2:
			if not redified:
				redified = True
				c1 = f"{Fore.RED}{c1}"
				if c2 != '':
					c2 = f"{Fore.RED}{c2}"
		diffX = diffX + c1
		diffY = diffY + c2
	extra = y[i+1:]
	if not redified:
		redified = True
		extra = f"{Fore.RED}{extra}"
	diffX = f"{diffX}{Style.RESET_ALL}"
	diffY = f"{diffY}{extra}{Style.RESET_ALL}"
	return (diffX, diffY)

def assertAsStrEqual(a, b, itemName="item", compare=identityCompare):
	x = str(a)
	y = str(b)
	if (compare(x, y)):
		return
	(diffX, diffY) = computeStringDifferences(x, y)
	raise Exception(f"Assertion for {itemName} failed: \n\t{Style.BRIGHT}Expected{Style.RESET_ALL}:\n\t{diffX}\n\t{Style.BRIGHT}Got{Style.RESET_ALL}:\n\t{diffY}")
