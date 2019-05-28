import tkinter
import getopt
import argparse
import sys
import asyncio
import time
import random

from typing import *




###########
###########
###########

print("\n\n---Python block is specified by indent---\n")


if True:
    print("1")
#  print("2" # Delete the first '#' on this line and you get an IndentationError exception)
else:
    print("3")
    print("4")




###########
###########
###########

print("\n\n---Check for variable existence---\n")

try:
    print("a=" + a)
except NameError:
    print("Variable 'a' is undefined")

if ('a' not in locals()) and ('a' not in globals()):
    print("Variable 'a' is still undefined")
else:
    print("a=" + a)




###########
###########
###########

print("\n\n---Modules---\n")

print("A module is basically a Python source that is imported. " \
      "A package is a collection of modules; __init__.py is called " \
      "when the module is loaded.")




###########
###########
###########

print("\n\n---Typing (since 3.5)---\n")

def greeting(s: str) -> str:
    print("Hello", s)

greeting("World")
greeting(3)
# These types are not very useful, as no checks are performed

def sum(a: int) -> int:
    return 3 + a

try:
    # No static check preventing me from running this
    print("sum with string", sum("a"))
except TypeError as e:
    print("can't sum a string with a number:", e)




###########
###########
###########

print("\n\n---Parse arguments---\n")

try:
    ap = argparse.ArgumentParser(description="This is a test program")
    ap.add_argument("-t", help="Dummy param (short version)")
    ap.add_argument("--test-param", help="Dummy param (long version)")
    args = ap.parse_args()
    print("Arguments as returned by ArgumentParser.parse_args(): ", str(args))
    if args.t is None:
        print("'t' argument was not given")
    else:
        print("'t' argument has value '" + args.t + "'")
    a = "<" + args.t + ">" # This line generates the TypeError exception caught below
    print(a)
except TypeError:
    print("Some arguments are undefined")




###########
###########
###########

print("\n\n----Parse arguments v2--\n")

print("Args are ", str(sys.argv))

# "opts, args" is a tuple
opts, args = getopt.getopt(sys.argv[1:], "t:", ["test-param="])

print("Opts is ", str(opts))

for opt, val in opts:
    print("Opt is '", opt, "', val is '", val, "'")
    if opt in ("-t", "--test-param"):
        print("Value of test param is '", val, "'")




###########
###########
###########

print("\n\n----String formatting--\n")

a = 5
b = 6
try:
    print("a = " + a)
    # This generates a TypeError: cannot concatenate 'str' and 'int' objects
except TypeError:
    print("You cannot concatenate a string and an int")

print("Percent formatting: a = %d, b = %d" % (a, b))
print("str.format(): a={}, b={}".format(a+1, b+1))
print(f"'f' strings: a={a+2}, b={b+2}")





###########
###########
###########

print("\n\n---String indexing---\n")

s = "0123456"
print(s[0:2])
print(s[:2])
print(s[1:3])




###########
###########
###########

print("\n\n---Lists (and generator expression)---\n")

a = [i*2 for i in range(1,11) if i%2 == 0]
print("List created with list comprehension and range(): ", a)

a = (i*2 for i in range(1,11) if i%2 == 0)
print("'List' created with generator and range(): ", a)
# Note the syntax is almost identical to list comprehension ... only [] and () differ
b = ''
for i in a:
    b = f'{i} plus {b}'
print(f"The generator 'list' constructed via a 'for': {b}")

print("Combination of 2 elems based on criteria: ")
print("\tWith list comprehension: ", \
    [(x, y) for x in [1,2,3] for y in [3,1,4] if x != y])

combs = []
for i in [1,2,3]:
    for j in [3,1,4]:
        if i != j:
            combs.append((i,j))
print("\tWith plain for loops:    ", combs)




###########
###########
###########

print("\n\n---Sets---\n")

s = {'D', 'u', 'd', 'e'}
s1 = {'u', 'e'}

if s1 < s:
    print(s1, "is included in", s)




###########
###########
###########

print("\n\n---Dictionaries---\n")

d = {"two": 2, "three": 3, "one": 1, "four": 4}

# Dictionaries preserve creation order
print("d keys:", list(d.keys()))

# Dictionaries return "views", which can be iterated or tested for membership
for [k, v] in d.items():
    print("item ", k, " - ", v)

# Sort them if you please
for k in sorted(d.keys()):
    print("sorted item ", k, " - ", d[k])




###########
###########
###########

print("\n\n---Asterisk---\n")

def doStuff(n):
    return n + 1

print("Expand function call tuple", *(doStuff(i) for i in range(3)))




###########
###########
###########

print("\n\n---Objects---\n")

# Function to display info about objects
def info(o):
    methodList = [method for method in dir(o) \
        if callable(getattr(o, method))]
    print("Methods of " + str(type(o)) + " '" + str(o) + "' are: ", methodList)

info(1)
info(o="a")

print("capital letter ", "a".capitalize())

# Call the same method by introspection
methodName = "capitalize"
method = getattr("a", methodName)
print("capital letter by introspection ", method())




###########
###########
###########

print("\n\n---Lambda functions: unnamed functions defined inline---\n")

caps = 1
f = caps and (lambda s: s.capitalize()) or (lambda s: s.lower())
print("caps is " + str(caps) + ": " + f("b"))




###########
###########
###########

print("\n\n---Classes---\n")

# Define a dummy class
# "pass" tells Python to do nothing (sort of "nop")

class Dummy:
    pass


# A class with an initialization function that is called automatically after
# constructing the object. It is not a constructor;
# the object is already constructed when __init__ gets called.

class Loaf:
    # class member
    count = 0
    def __init__(self):
        self.value = "3"

print("Loaf.count is " + str(Loaf.count))
l = Loaf()
print("l is " + str(l))
print("l.value is '" + str(l.value) + "'")
l.value = 2
l.value += 3
print("l.value is now '" + str(l.value) + "'")
l.value = "a"
print("l.value is now '" + str(l.value) + "'")


# Special class methods
# __getitem__ gets called when accessing a CustomIndex variable with '[]'
# __setitem__ is similar, but lets you modify the variable accesed with '[]'
# __call__ is called when you call a class like a function (!)

class CustomIndex:
    def __init__(self):
        print("Constructing ", self)
        self.str = "a"
    def __getitem__(self, key): return self.str
    def __setitem__(self, key, item): self.str = item
    def __call__(self, param):
        print("" + str(param) + " " + str(self) + " was called as a function")

d = CustomIndex()
print("d[1] is " + d[1])
d[1] = "b"
print("d[1] is now " + d[1])
d[2] = "c"
print("d[1]: " + d[1] + "; d[2]: " + d[2] + " (this index is dumb)")
d("gigi")
print("d[n] is " + d["n"])


# Private attributes (variables, functions) begin with __ AND don't end with __

class Priv:
    def __init__(self):
        self.__private_member = "I am private"
        self.public_member = "I am a public member"
    def show(self):
        print("I am a public method")
        print("My object has the following members:")
        print("\t" + self.__private_member)
        print("\t" + self.public_member)
        self.__internal()
    def __internal(self):
        print("I am a private method")
    def __public_method__(self):
        # Multi-line string similar to C
        print("I am a public method. Don't give me such a name, "
            "it will cause confusion with special class methods.")

p = Priv()
p.show()
# print(p.__private_member # Uncomment this to get an AttributeError exception)
print(p.public_member)
# p.__internal() # Uncomment this to get an AttributeError exception
p.__public_method__() # Because it also ENDS with __, this is not private




###########
###########
###########

print("\n\n---Exception Handling---\n")

try:
    fsock = open("/nofile", "r")
except IOError as e:
    print("IOError exception:", e)
print("Is file opened or not?")




###########
###########
###########

print("\n\n---Generators---\n")

def generateSome():
    print("Generator: Step 1")
    yield 243
    print("Generator: Step 2");
    yield 244

it = generateSome()
print(f"Generator: returned value 1 {next(it)}")
print(f"Generator: returned value 2 {next(it)}")
#print(f"Generator: returned value 3 {next(it)}")





###########
###########
###########

print("\n\n---Async IO---\n")

#
# Async version of 'Hello World'
# 

async def count(n):
    sleepSeconds = 1 / (n - 1)
    for i in range(1, n):
        print(f"Count {i}")
        await asyncio.sleep(sleepSeconds)

async def asyncRoutine():
    await asyncio.gather(count(3), count(4), count(5))

s = time.perf_counter()
asyncio.run(asyncRoutine())
elapsed = time.perf_counter() - s
print(f"async hello executed in {elapsed:0.2f} seconds.")

#
# How async calls are interlaced
#
  
# ANSI colors
c = (
    "\033[0m",   # End of color
    "\033[36m",  # Cyan
    "\033[91m",  # Red
    "\033[35m",  # Magenta
)
async def makerandom(idx: int, threshold: int = 6) -> int:
    print(c[idx + 1] + f"Initiated makerandom({idx}).")
    i = random.randint(0, 10)
    while i <= threshold:
        print(c[idx + 1] + f"makerandom({idx}) == {i} too low; retrying.")
        await asyncio.sleep(idx + 1)
        i = random.randint(0, 10)
    print(c[idx + 1] + f"---> Finished: makerandom({idx}) == {i}" + c[0])
    return i

async def asyncRandom():
    res = await asyncio.gather(*(makerandom(i, 10 - i - 1) for i in range(3)))
    return res

random.seed(444)
r1, r2, r3 = asyncio.run(asyncRandom())
print()
print(f"r1: {r1}, r2: {r2}, r3: {r3}")

#
# Chaining async calls
#

async def part1(n: int) -> str:
    i = random.randint(0, 10)
    print(f"part1({n}) sleeping for {i} seconds.")
    await asyncio.sleep(i)
    result = f"result{n}-1"
    print(f"Returning part1({n}) == {result}.")
    return result

async def part2(n: int, arg: str) -> str:
    i = random.randint(0, 10)
    print(f"part2{n, arg} sleeping for {i} seconds.")
    await asyncio.sleep(i)
    result = f"result{n}-2 derived from {arg}"
    print(f"Returning part2{n, arg} == {result}.")
    return result

async def chain(n: int) -> None:
    start = time.perf_counter()
    p1 = await part1(n)
    p2 = await part2(n, p1)
    end = time.perf_counter() - start
    print(f"-->Chained result{n} => {p2} (took {end:0.2f} seconds).")

async def asyncChain(*args):
    await asyncio.gather(*(chain(n) for n in args))

random.seed(444)
args = [9, 6, 3]
start = time.perf_counter()
asyncio.run(asyncChain(*args))
end = time.perf_counter() - start
print(f"async chain finished in {end:0.2f} seconds.")





###########
###########
###########

print("\n\n---Graphics with Tkinter---\n")

master = tkinter.Tk()
w = tkinter.Canvas(master, width=200, height=100)
w.pack()
w.create_line(0, 0, 200, 100)
w.create_line(0, 100, 200, 0, fill="red", dash=(4, 4))
w.create_rectangle(50, 25, 150, 75, fill="white")
t = w.create_text(100, 50, text="Hello")

tkinter.mainloop()

# raw_input("\n\nPress ENTER to exit")

