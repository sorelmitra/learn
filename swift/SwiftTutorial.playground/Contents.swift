//: Swift tutorial: a quick starter on Swift based on a tutorial from raywenderlich.com and on "The Swift Programming Language" book by Apple. By Sorel Mitra

import UIKit


//////////////////////////////////////////////////////////////////
// Variables and Constants

var str = "Hello, playground" // inferred type string
var nsStr = str as NSString // same string, converted to NSString

let tutorialTeam = 60 //constant
let editorialTeam = 17 // constant
var totalTeam = tutorialTeam + editorialTeam // variable
totalTeam += 1

let nameInferred = "Whoopie Cushion"
let nameExplicit: String = "Whoopie Cushion" // same as nameInferred
let price: Double = 17.1 // specified type again
let priceOnSale = 15.7 // double, inferred by compilator


///////////////////////////////////////////////////////////////////
// Optional Values

// The question mark after a type indicates that the value it contains is optional. This means it either contains a <Type> value, or it contains no value at all. Containing no value is expressed with nil.
var optionalName: String? = "Dude"

///// The five ways of accesing an optional

///// 1: Accessing the Optional directly. You get a value or nil

println("Optional name is \(optionalName)")

///// 2: Conditionally transferring it to a non optional. You get a value, but you have to handle the "else"

if let name = optionalName {
    if name == "Dude" {
        println("This is a dude")
    }
    println("Hello, \(name)")
} else {
    println("Noname, hello!")
}

///// 3: Force Unwrapping an optional. You get a value, but the code will fail if the optional was nil

// The exclamation mark forces the runtime to unwrap the value from the optional
println("Force unwrapped name: \(optionalName!)")

///// 4: Optional Chaining. You get a value, but the code will silently return nil if the optional is nil

// Force unwrapping vs Optional chaining a nil
optionalName = nil
//println(optionalName!) // uncomment to get a fatal error
//println(optionalName!.isEmpty) // uncomment to get a fatal error
//println(optionalName?) // uncomment for compilation error - you must access something here - a member or a subscript
println(optionalName?.isEmpty) // prints nil and returns nil

// Usefulness of optional chaining: it allows conditionally transferring optional members of a class to a non-optional
class Address {
    var street: String = ""
}
class Person {
    var address: Address?
}
let john = Person()
//println("John's name is \(john.address!.street)") // uncomment for fatal error as john.address is nil
// optional chaining
if let street = john.address?.street {
    println("John's street is \(street)") // uncomment for fatal error as john.address is nil
} else {
    println("Unable to get John's street")
}

///// 5: Implicit unwrapping

let optNr: Int? = 6
let implicitNr1: Int! = optNr! // you get 6
let implicitNr2: Int! = optNr // you get 6 again
let optNil: Int? = nil
var implicitNr3: Int! = optNil // you get nil
println("Implicit optional nil: \(implicitNr3)")
implicitNr3 = nil
println("Implicit optional nil: \(implicitNr3)")

/*
Discussion: nil can only be used with optional values.

To me, this looks like pointers in C, with the added bonus that you don't have to manage the memory, and you don't get the ugly errors that you get with pointers in C.

- A normal Swift variable always has a value - this is similar to a non-pointer variable in C.
- A Swift Optional may have a value or may not have a value (nil) - this is similar to a pointer in C (which may point to an address or be null). This differs from C pointers in that you cannot get a Swift optional to "point to a bad address" like it happens with C pointers.

- Accesing the optional directly, and accesing it conditionally are similar to accesing C pointers this way (without the ugly errors in case of a bad pointer).
- Force unwrapping the conditional is similar to dereferencing a pointer in C (including getting a runtime error if there's no value, but without the ugly errors...).
- Optional chaining has no similarity with C pointers. It's very similar to force unwrapping (and thus to dereferencing C pointers), but it silently makes the whole expression return nil if the optional is nil
*/


/////////////////////////////////////////////////////////////////////
// Arrays and Dictionaries

let arr = [1, 3, 5]
var dict = [1: "One", 3: "Three", 5: "Five"]

// Iterate an array
var total = 0
for i in arr {total += i}

// Iterate a dictionary
for (nr, literal) in dict {
    println("\(literal) is \(nr)")
}

// Access dictionary by key - the value is optional (see above)
println(dict[3])

// Remove item from dictionary
dict[3] = nil
println(dict[3])

// Cannot remove from constant dictionary (declared with "let").  Uncomment below for errors
let constDict = [1: "One", 3: "Three", 5: "Five"]
/*
constDict[3] = nil
constDict.removeValueForKey(3)
*/


/////////////////////////////////////////////////////////////////////
// If

let onSale = false
if onSale { // parantheses are optional for "if"
    println("\(nameInferred) on sale for \(priceOnSale)")
} else {
    println("\(nameInferred) on sale for \(price)")
}

// Any string can do interpolation of expressions with \(), not just print()
let interpolatedString = "Sale price difference is \(price - priceOnSale)"
println(interpolatedString)


// "name" is not available outside the if; uncomment for error
//println(name)

///////////////////////////////////////////////////////////////////
// Control Flow

///// for loop
print("for loop with non-inclusive operator: ")
for i in 0 ..< 5 {
    print("\(i) ")
}
println()

print("for loop with inclusive operator: ")
for i in 0 ... 5 { // "..." is the inclusive operator
    print("\(i) ")
}
println()

print("classic for loop: ")
for var i = 0; i < 5; i++ {
    print("\(i) ")
}
println()

///// switch
let vegetable = "red pepper"
switch vegetable { // you can switch strings
case "celery":
    let vegetableComment = "Add some raisins and make ants on a log."
case "cucumber", "watercress": // same behavior for multiple values
    let vegetableComment = "That would make a good tea sandwich."
case let x where x.hasSuffix("pepper"):
    // more complex case expression - "x" can be any name and applies to the value passed to swtich
    let vegetableComment = "Is it a spicy \(x)?"
// You must have a default clause in any switch
default:
    let vegetableComment = "Everything tastes good in soup."
}

///// while
var n = 2
while n < 100 {
    n = n * 2
}
println(n)


/////////////////////////////////////////////////////////////////////
// Functions and Closures

///// "normal" function; return type is what follows the "->"
func returnFifteen() -> Int {
    var y = 10
    // "nested" function
    func add() {
        y += 5 // have access to var declared in outer function
    }
    add()
    return y
}
returnFifteen()

///// function that modifies its parameters; it must be called with "&" before the parameters
func swapTwoInts(inout a: Int, inout b: Int) {
    let temporaryA = a
    a = b
    b = temporaryA
}

var someVal = 8
var anotherVal = 9
swapTwoInts(&someVal, &anotherVal)
println("someVal=\(someVal), anotherVal = \(anotherVal)")

///// function as type: this function returns another function which is treated as a type
func makeIncrementer() -> (Int -> Int) { // "(Type -> Type)" is a type decl for a func
    func addOne(number: Int) -> Int {
        return 1 + number
    }
    return addOne // return the function
}
var increment = makeIncrementer() // "increment" is actually a function
increment(7) // call the function

///// function as argument to another function: "condition" is a function type, declared as "Type -> Type" - compare to the above makeIncrementer, where the function type was put in paranthesis; apparently, the paranthesis is not required, it only eases readability for humans; try removing them at the return type of makeIncrementer
func hasAnyMatches(list: [Int], condition: Int -> Bool) -> Bool {
    for item in list {
        if condition(item) {
            return true
        }
    }
    return false
}
// this function can be passed as a "condition" above, as it's type matches
func lessThanTen(number: Int) -> Bool {
    return number < 10
}
var numbers = [20, 19, 7, 12]
hasAnyMatches(numbers, lessThanTen)

///// closure: the "map" function takes as parameter another function which takes a type and returns another type; in this case, it takes an Int (whose name is "number") and returns an int
// The closure format is: "{Func-Type in Body}", where Func-Type is "(var-name: Type) -> Type", and Body contains Swift instructions
let multipliedNumbers = numbers.map({
    (number: Int) -> Int in // "Func-Type in"
    let result = 3 * number // Body
    return result  // Body
}) // This whole instruction multiplies by 3 each value in "numbers"
println(multipliedNumbers)

// short version of the above closure
println(numbers.map({number in number * 4}))

// even shorter version, using numbered instead of named parameters:
println(numbers.map({$0 * 5}))

// the long version again, but with numbered params - uncomment for error
/*
println(numbers.map({
    (Int) -> Int in // "Func-Type in"
    let result = 6 * $0 // ERROR: Cannot use numbered param when specific arguments have been declared
    return result  // Body
}))
*/

// another closure with numbered params
println(numbers.sorted({$0 < $1}))
// different syntax for the same "sorted" thing
let sortedNumbers = sorted(numbers) { $0 > $1 }
println(sortedNumbers)


//////////////////////////////////////////////////////////////////
// Classes

class TipCalculator { // "tip", as in a sum of money given to a restaurant waiter
    let total: Double // uninitialized constant with specified type
    let taxPercent: Double
    let subtotal: Double

    static func classMethod() {
        println("This is a class method")
    }

    init(total: Double, taxPercent: Double) {
        self.total = total
        self.taxPercent = taxPercent
        subtotal = total / (taxPercent + 1)
    }

    // method declaration; return type is what follows the "->"
    func calcTipWithTipPercent(tipPercent: Double) -> Double {
        return subtotal * tipPercent;
    }

    func printPossibleTips() {
        let possibleTips = [0.15, 0.18, 0.20]
        for tip in possibleTips {
            println("\(tip * 100)%: \(calcTipWithTipPercent(tip))")
        }
    }

    // Dictionary is specified as "[Key-Type: Value-Type]"
    func returnPossibleTips() -> [Int: Double] {
        let possibleTips = [0.15, 0.18, 0.20]
        var tipValues = [Int: Double]()
        for tip in possibleTips {
            let intPercent = Int(tip * 100)
            tipValues[intPercent] = calcTipWithTipPercent(tip)
        }
        return tipValues
    }
}

// you can define operators, like in C++
func += (left: TipCalculator, right: TipCalculator) -> TipCalculator {
    return TipCalculator(
        total: left.total + right.total,
        taxPercent: left.taxPercent + right.taxPercent)
}

TipCalculator.classMethod()

// Instantiate class by putting "()" after the class name
let tipCalc = TipCalculator(total: 33.25, taxPercent: 0.06)
tipCalc.printPossibleTips()
let tipValues = tipCalc.returnPossibleTips()
println(tipValues)

// Class intended to be a base class
class NamedShape {
    var numberOfSides: Int = 0 // property/member variable must be initialized
    var name: String // if the property is not initialized here, then it must be initialized in init()

    // "init" does not have "func" before, it seems to be a sort of a constructor
    init(name: String) {
        self.name = name
    }

    // destructor?
    deinit { // when does this get called?
        println("deinit() called")
    }

    func simpleDescription() -> String {
        return "\(self.name): a shape with \(numberOfSides) sides."
    }
}

let shape = NamedShape(name: "base shape")
shape.simpleDescription()

// Derived class
class Square: NamedShape {
    var sideLength: Double

    init(sideLength: Double, name: String) {
        self.sideLength = sideLength
        super.init(name: name)
        numberOfSides = 4
    }

    func area() ->  Double {
        return sideLength * sideLength
    }

    // You must express the intention to override a function.  Delete the "override" below to get an error about unintended override
    override func simpleDescription() -> String {
        return "A square with sides of length \(sideLength)."
    }

    // You can not "override" functions that are not in the base class.  Uncomment below to get error
    /*override func perimeter() -> Double {
        return sideLength * 4;
    }*/
}
let square = Square(sideLength: 5.2, name: "my test square")
square.area()
square.simpleDescription()


//////////////////////////////////////////////////////////////////
// Variable Observers and Computed Variables

class EquilateralTriangle: NamedShape {
    var sideLength: Double = 0.0 {
        // Any stored (i.e. not computed) property can have one of two observers
        willSet {
            println("old side length was \(sideLength), new value is \(newValue)")
        }
        didSet {
            // as with computed properties (see below), oldValue could be renamed by adding a name in paranthesis after the observer: didSet(myOldSideLength) {...}
            println("new side length is \(sideLength), old value is \(oldValue)")
        }
    }

    var wrongObserver = 5 {
        didSet {
            // trap: if you set a variable's value inside its own didSet observer, then the value you set replaces the vaue given
            self.wrongObserver = 7
        }
    }

    init(sideLength: Double, name: String) {
        // you must initialize derived class members before calling super.init()
        self.sideLength = sideLength
        super.init(name: name)
        // base class members can be initialized only after calling super.init()
        numberOfSides = 3
        println("perimeter inside init: \(self.perimeter)")
    }

    // Computed property: it's not stored, but rather computed from other stored members
    var perimeter: Double {
        get {return 3.0 * sideLength}
        // Computed properties cannot have observers
        /*willSet {
            println("old side length was \(sideLength)")
        }*/
        // newValue is the implicit name of set's parameter
        set {sideLength = newValue / 3.0}
        // alternative: give the name in paranthesis after set
        //set(perim) {sideLength = perim / 3.0}
    }

    override func simpleDescription() -> String {
        return "An equilateral triangle with sides of length \(sideLength)."
    }
}

var triangle = EquilateralTriangle(sideLength: 3.1, name: "a triangle")
println("perimeter after init: \(triangle.perimeter)")
triangle.perimeter = 9.9
println("side length after setting perimeter: \(triangle.sideLength)")
triangle.sideLength = 4.5
println("perimeter after setting side length: \(triangle.perimeter)")
let a = 9
triangle.wrongObserver = a
println("value of variable with the wrong observer: \(triangle.wrongObserver), expected \(a)")

// Global variables can have observers, too.  You must specify a type and a default value for these globals
var varWithObservers: Int = 0 {
    didSet {
        println("old value of global var with observer: \(oldValue)")
    }
}
varWithObservers = 5

// Global variables can have observers, too.  You must specify a type and a default value for these globals
var computed: String {
    get {return "b"}
    set {println("I refuse setting to \(newValue)")}
}

println(computed)
computed = "a"
println(computed)


//////////////////////////////////////////////////////////////////
// External Parameter Names and Local Parameter Names

class A {
    // For class Methods, the default behavior is that the first parameter is local and the rest of them are external
    func f(firstArg: Int, secondArg: Double, thirdArg: String) {
        println("first \(firstArg) second \(secondArg) third \(thirdArg)")
    }

    // This method has explicitly defined the first argument as external with the same name
    func g(firstArg firstArg: Int, secondArg: Double, thirdArg: String) {
        println("first \(firstArg) second \(secondArg) third \(thirdArg)")
    }

    // This method has also defined the first argument as external, but with a more succint syntax (parameter name prefixed by #)
    func h(#firstArg: Int, secondArg: Double, thirdArg: String) {
        println("first \(firstArg) second \(secondArg) third \(thirdArg)")
    }

    // This method has defined the first argument as external with a different name.
    // The third argument is not external anymore, because it has "_" as the external parameter name
    func i(first firstArg: Int, secondArg: Double, _ thirdArg: String) {
        println("first \(firstArg) second \(secondArg) third <\(thirdArg)>")
    }
}

let aa = A()
// When calling the method, you must put labels for all arguments except the first one
aa.f(1, secondArg: 3.0, thirdArg: "default arguments")
// You cannot put label for the first argument, uncomment for error
//aa.f(firstArg: 1, secondArg: 5, thirdArg: "blah")
// You cannot miss a label for a non-first argument, uncomment for error
//aa.f(1, secondArg: 7.7, "gu")

// Calling the functions that define the first argument as external
aa.g(firstArg: 2, secondArg: 4.6, thirdArg: "explicit external, same name")
aa.h(firstArg: 3, secondArg: 4.7, thirdArg: "succint external, same name")
// No label for the third argument here
aa.i(first: 4, secondArg: 4.8, "external with different name than local")

// Having both external and internal parameter names helps both the caller and the function body to be more succint
func join(string s1: String, toString s2: String, withDelimiter d: String) -> String {
    return s1 + d + s2;
}
println(join(string: "Hi", toString: "you", withDelimiter: ", "))

// A parameter can have a default value, in which case it can be ommitted.  The parameter with default value can be placed anywhere, but it's recommended to be placed at the end. The compiler automatically provides an external name to parameters with default values.
func connect(string s1: String, via: String = "", toString s2: String) -> String {
    return s1 + via + s2;
}
println(connect(string:"Hi", toString: "there"))
// Giving explicit value to parameter with default one; must specify label
println(connect(string:"Hi", via: " ", toString: "there"))

// Put an underscore "_" as the external name of a parameter with default name to prevent it from having an external name
func connect_2(string s1: String, _ via: String = "", toString s2: String) -> String {
    return s1 + via + s2;
}
// Now you may not give a label to the param with default value
println(connect_2(string:"Hi", " ", toString: "there"))


//////////////////////////////////////////////////////////////////
// Enumerations

// An enum has a type
enum Rank: Int {
    case Ace = 1
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten
    // Jack is 11
    case Jack, Queen, King
    // You can have methods in an enum
    func simpleDescription() -> String {
        switch self {
        case .Ace:
            return "ace"
        case .Jack:
            return "jack"
        case .Queen:
            return "queen"
        case .King:
            return "king"
        default:
            return String(self.rawValue)
        }
    }
}

let r = Rank.Queen
println("Queen is \(r.rawValue)")

// Enums can also have un-meaningful raw values

enum Weapons {
    case Sword, Arch
    case Gun, Cannon
    func isFireWeapon() -> Bool {
        switch(self) {
        case .Sword, .Arch:
            return false
        default:
            // You cannot ommit the default and place the return below after the switch, like you do in C.  Switch must be exhaustive
            return true
        }
    }
}
var w = Weapons.Sword
println("is fire weapon: \(w.isFireWeapon())")
// There's no raw value here - uncomment for error
//println("Sword is \(w.rawValue)")

// Now that w is of type Weapons, we can use a shorter dot syntax to change its value
w = .Gun
println("is fire weapon: \(w.isFireWeapon())")

// Cannot use a value from a different enum - uncomment for error
//w = .Queen

// Structs are similar to classes, but they are passed by reference
struct X {
    var l: Int
    var l2: Int
    func sumUp() {
        println("sum is \(l + l2)")
        // You cannot modify a struct or enum from a member function - uncomment for error
        //l = 8
    }
    // You CAN modify a struct or enum if you declare the method "mutating"
    mutating func modify() {
        l = 8
    }
}

class Y {
    var l: Int = 0
    // You CAN modify a class from a member function
    func modify() {
        l = 5
    }
}

let xw = X(l: 3, l2: 7)
xw.sumUp()


//////////////////////////////////////////////////////////////////
// Protocols

// A protocol defines requirements for properties, methods, etc. It is simlar to an interface in Java
protocol SomeProtocol {
    // Property requirements are always declared as variable properties, prefixed with the var keyword. Gettable and settable properties are indicated by writing { get set } after their type declaration, and gettable properties are indicated by writing { get }.
    var mustBeSettable: Int { get set }
    var doesNotNeedToBeSettable: Int { get }
    // static func requirement
    static func someTypeMethod()
    // method requirement
    func doStuff(thing: String) -> String
    // requirement for a method that can modify the object it belongs to; declaring it like this enables structures and enums to adopt the protocol
    mutating func modifyMe()
    // requirement for an init function
    init(someParameter: Int)
}

protocol Proto {
    func did()
}

class Base {
}

// A class that derives from another class and adopts a protocol; the base class is specified first, then the adopted protocols are listed
class AdoptsProto: Base, Proto {
    func did() {
        println("I did it")
    }
}


//////////////////////////////////////////////////////////////////
// Extensions

protocol DoubleAbs {
    func absoluteValue() -> Double
}

// An extension adds functionality to an existing class, even if we don't have the source of that class
extension Double: DoubleAbs {
    func absoluteValue() -> Double {
        if self < 0 {
            return -self
        }
        return self
    }
}

// Now Double type has our extension
let d: Double = -7.6
println("abs(\(d)) = \(d.absoluteValue())")

// You can use a protocol name just like any other named type—for example, to create a collection of objects that have different types but that all conform to a single protocol. When you work with values whose type is a protocol type, methods outside the protocol definition are not available.
let da: DoubleAbs = d
// Uncomment for error
//da.description


//////////////////////////////////////////////////////////////////
// Generics

// Generic function to swap two values, no matter the type of those values
func swapTwoValues<T>(inout a: T, inout b: T) {
    let temporaryA = a
    a = b
    b = temporaryA
}

var ag = "A"
var bg = "B"
swapTwoValues(&ag, &bg)

// A generic repeater
func repeat<Item>(item: Item, times: Int) -> [Item] {
    var result = [Item]()
    for i in 0..<times {
        result.append(item)
    }
    return result
}
repeat("knock", 4)

// Generic type
struct Stack<T> {
    var items = [T]()
    mutating func push(item: T) {
        items.append(item)
    }
    mutating func pop() -> T {
        return items.removeLast()
    }
}

var st = Stack<String>()
st.push("bottom")

// You can extend a generic type
extension Stack {
    var topItem: T? {
        return items.isEmpty ? nil : items[items.count - 1]
    }
}

var st2 = Stack<Int>()
st2.push(4)
println(st2.topItem)

// Generic type constraints
func someFunction<T: Base, U: Proto>(someT: T, someU: U) {
    // Type T must be a Base or derived from Base
    // Type U must adopt the Proto protocol
}

// Usefulness of type constraints: the code below would not compile without the Equatable constraint, because not all classes might have the == and != operators defined
func findIndex<T: Equatable>(array: [T], valueToFind: T) -> Int? {
    for (index, value) in enumerate(array) {
        if value == valueToFind {
            return index
        }
    }
    return nil
}
