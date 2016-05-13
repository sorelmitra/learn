/*****************************************************************
 * 
 * Closures
 * 
 *****************************************************************/

function makeAdder(increment) {
    // The function "add" has access to the "increment" variable (it has a closure to that variable); this variable is accessible to "add" function even after the makeAdder function has finished
    // From "Thinking in Java": A closure is a callable thing that retains information from the scope in which it was created
    // Multiple calls to makeAdder create multiple instances of the "add" function
    function add(to) {
        var sum = to + increment;
        console.log("%d + %d = %d", to, increment, sum);
        to = sum;
    }
    // The changeIncrement function also has access to the same "increment" variable after makeAdder has finished
    function changeIncrement(newIncrement) {
        increment = newIncrement;
    }
    // When the caller of makeAdder saves the "add" and changeIncrement functions, it will have functions that have a reference to the same "increment" variable that was created when invoking this function. Further calls to makeAdder will create other "increment" variables 
    return [add, changeIncrement];
}

// We get an array of functions: an "add" function instance, that has a reference to an "increment" variable, whose value is 1, and a changer function, that allows for changing the increment variable
var adder = makeAdder(1);
var plusOne = adder[0];
var changePlusOne = adder[1];
// We get another array of functions: an "add" function instance, that has a reference to another "increment" variable, whose value this time is 10, and a changer function, that allows for changing the second increment variable
adder = makeAdder(10)
var plusTen = adder[0];

plusOne(5); // Adds 1 to 5
plusTen(7); // Adds 10 to 7

// Now let's ruin plusOne by changing its increment...
changePlusOne(2);
plusOne(5); // Adds 2 to 5 this time...

plusTen(7); // Still adds 10 to 7, because plusOne and plusTen have references to different instances of the "increment" variable; to change plusTen's increment we would have to retain a reference to its changer function from the "adder" array above, and call it - or call it directly from the array:
adder[1](9); // call plusTen's increment changer directly from the array returned by makeAdder
plusTen(7); // Now we've ruined plusTen, too: it adds 9 to 7


//// Demonstration that a function has a "closure" on the variables in its scope, even when those variables are gone once their scope is finished

function foo() {
    var a = 2;

    function bar() {
        console.log("I have a closure on this variable whose value is %d", a);
    }

    return bar;
}

var baz = foo();

baz(); // 2 -- Whoa, closure was just observed, man.
// Calling bar via baz as above produces 2 because bar is a callable thing (a function) that retains information (e.g. variables) from the scope in which it was created (the scope of foo())

// console.log("I'm trying to access that variable that baz (which is a reference to bar) said to have a closure on, a = ", a); // ReferenceError, a is not declared in this scope; although baz still has access to it thanks to its closure (=reference) on it


//// Another closure example from the YDKJS book - trying to print incremented numbers at 1-second intervals using setTimeout

var timeout;

timeout = 100;
setTimeout(function log1() {
    console.log("Try 1 I'm not counting numbers correctly")
}, timeout);
for (var i=1; i<=5; i++) {
    setTimeout( function timer(){
        console.log("Try 1 No %d", i);
    }, timeout + i );
}
// The above code does not count correctly, because each call to setTimeout gives it a new function called "timer", and each "timer" functions have a closure on the same "i" variable. Whenever a "timer" function is called, it uses whatever value is in "i" at that time. By the time the for loop finishes, no timer function would have been called (not even if the interval would be 0). So when all timer functions are called, i is already 6

// Let's fix it. The idea is to create a new scope for each "timer" function, scope that would make sure each "timer" function retains a reference to a variable with the proper value. To achieve this we use an Immediately Invoked Function Expression (IIFE), that creates a new scope. In this new scope we declare a new variable that has its value set to i's value. Because the IIFE is executed right-away, it will save in j the value of i at the moment of creating the timer. And since in the "timer" function we now use j, it will have the correct value. Since each time the loop is entered a new IIFE is created and executed, we effectively have 5 IIFEs, which create 5 scopes, each one with a "j" variable and a "timer" function passed to setTimeout
// In ECMAScript 6 and above, we would have achieved the same efect *without* and IIFE but replacing "var j" with "let j". "let" turns a scopeless block into a scope (normally, only function blocks have a scope)
timeout = 200;
setTimeout(function log2() {
    console.log("Try 2 Now I'm counting numbers correctly")
}, timeout);
for (var i=1; i<=5; i++) {
    (function createAVariableInANewScope() {
        var j = i; // use "let" instead of "var" here and the code works without the createAVariableInANewScope function declaration
        setTimeout( function timer(){
            console.log("Try 2 No %d", j); // Must use j here, as j is set to the current value of i, and it will keep its value for the "timer" function
        }, timeout + i ) // I don't need to use j here as this expression is executed right away. I need to use j only in the function that will be executed later, function that will make use of the closured variable
    })();
}

// As stated above, in ECMAScript 6 we can use "let" to declare j and solve the problem without the need for an IIFE. Even better, if we use "let" in the for loop, the variable "i" will be declared not just once for the loop, but once for each iteration, setting it with the right value. This way the original code works in ECMAScript 6 if we just replace "var" with "let":
timeout = 300;
setTimeout(function log3() {
    console.log("Try 3 I'm counting numbers correctly using 'let' in ECMAScript 6")
}, timeout);
for (let i=1; i<=5; i++) {
    setTimeout( function timer(){
        console.log("Try 3 No %d", i);
    }, timeout + i );
}


//// Closures used to create modules

function Robot() {
    var name;
    var ability;
    
    // closure function: it has references to the name and ability variables that were created when Robot() was called; multiple calls to Robot() create multiple instances of the name and ability variables
    function doInit(_name, _ability) {
        name = _name;
        ability = _ability;
    }
    
    // similar closure function
    function doPerform() {
        console.log("%s is executing %s", name, ability);
    }
    
    // public interface: we define two function variables, and assign them to our implementations; this way we can change the implementation function by simply modifying the values of the function variables to point to some other functions
    var robot = {
        init: doInit,
        perform: doPerform
    }
    
    // return the public interface, so that the caller of this function gets an object that can do stuff
    return robot;
}

var robot = Robot();
robot.init("robo", "cleaning");
robot.perform();

// robot.doInit("illegal", "illegal"); // Fires a TypeError exception: doInit is not accessible


/*****************************************************************
 * 
 * "this"
 * 
 *****************************************************************/


//// What "this" is NOT

var id = "not awesome";

(function wrongTimeout1() {
    var obj = {
        id: "awesome 1",
        cool: function coolFn() {
            console.log( this.id );
        }
    };

    obj.cool(); // awesome

    setTimeout( obj.cool, 400 ); // not awesome. "this" is NOT a reference to the "class" the function is "member of", because JavaScript is NOT Object-Oriented (at least at the time of this writing)
})();

(function wrongTimeout2() {
    var obj = {
        id: "awesome 2",
        cool: function coolFn() {
            console.log( this.id );
        },
        coolTimeout: function coolTimeoutFn() {
            setTimeout(this.cool, 500);
        }
    };

    obj.coolTimeout(); // not awesome. Trying to call setTimeout from "inside" the "class" yields the same result, for the same reason: JS is NOT OO
})();

(function rightTimeout() {
    var obj = {
        id: "awesome 3",
        cool: function coolFn() {
            console.log( this.id );
        },
        coolTimeout: function coolTimeoutFn() {
            setTimeout(this.cool.bind(this), 600);
        }
    };

    obj.coolTimeout(); // awesome 3. As is explained below, it works correctly because of "this" binding by using Function.prototype.bind()
})();


// "this" is a binding made in the activation record (execution context) of a function. The activation record is created at the call-site of the function, and makes "this" to point to the object from where the function is called. In JavaScript, everything is an object (except simple primitives - string, number, boolean, null, and undefined), including the global script (variables declared in the global scope are actually part of the global object) and functions.


//// 1. default binding
function defaultBinding() {
    console.log(this.defBind); // "this" points to the global object
}
var defBind = "(1) defBind is in fact <global object>.defBind (in browsers <global object> is window)";
defaultBinding(); // "(1) ..."

// default binding in strict mode
function defaultBindingInStrictMode() {
    "use strict";
    console.log( this.defBind ); // "this" is undefined, because in strict mode the global object is not eligible for default binding
}
// defaultBindingInStrictMode(); // TypeError: this is undefined, because of strict mode
(function immediatelyCalled() {
    "use strict";
    /*var*/ defBind = "(2) although we use strict mode here, the defaultBinding() function does not use strict mode, so 'this' is allowed to bind to the global object";
    // Note: if we uncomment the "var" above, we will get at runtime the value of defBind "(1) ..." instead of "(2) ..." (!!!). Why? Because the IIFE creates its own scope, and putting "var" above will create a new variable in the scope of this IIFE. See below when using a normal function for an explanation.
    defaultBinding();
})();

// default binding from another function
function usingDefaultBinding() {
    var defBind = "(3) I'm a function so I'm an object that can be passed as 'this'";
    defaultBinding();
}
usingDefaultBinding(); // Will we get "(3) ..." ? NO!!! because not LEXICAL SCOPE is what determines "this" binding, but the OBJECT used when calling the function. In our case, although we call the function from usingDefaultBinding, we don't pass the usingDefaultBinding Function object when calling defaultBinding. So DEFAULT binding is performed.

// default binding from another function
function usingFunctionObjectBindingExplicitly() {
    arguments.callee.defBind = "(4) Now we really pass the Function object as 'this'";
    // Note: We want to pass the function object as "this", so we need defBind to be a part of that function object. If we say "var defBind = ..." above instead of "arguments.callee.defBind = ...", then defBind will be a variable partaining to the functions local LEXICAL SCOPE. So we need to make defBind part of the function object itself, so we use the above syntax 
    defaultBinding.call(arguments.callee);
}
usingFunctionObjectBindingExplicitly();


//// 2. implicit binding, when calling from a context object

function showA() {
    console.log("a is ", this.a);
}

var obj = {
    a: 2,
    showA: showA
};

var obj2 = {
    a: 42,
    obj: obj
};

obj2.obj.showA(); // 2, only the last object matters

var defaultBindingByMistake = obj.showA; // function reference/alias!

var a = "oops, global"; // `a` also property on global object

defaultBindingByMistake(); // "oops, global", because we call it without a context object

setTimeout(obj.showA, 700); // "oops, global", because setTimeout() has a function reference which it calls without a context object

// another default binding by mistake
(obj2.showA = obj.showA)(); // "oops, global", as "this" points to the global object. Why did it get to the global object? Because the result value of the assignment expression p.foo = o.foo is a reference to just the underlying function object. As such, the effective call-site is just foo(), not p.foo() or o.foo() as you might expect


//// 3. explicit binding

showA.call(obj); // 2
showA.call(obj2); // 42

// Hard binding pattern
function showAOfObj() {
    return showA.apply(obj, arguments);
}

showAOfObj(); // 2
setTimeout(showAOfObj, 710); // 2, because of hard binding: showAOfObj() always passes obj as "this"

// Hard binding helper
function bind(func, obj) {
    return function() { // we return a function, as our purpose is to have a function that always has "this" set to obj
        return func.apply(obj, arguments); // the function we return simply calls the function we receive as the first argument with all arguments
    }
}

setTimeout(bind(showA, obj2), 720); // 42, because the hard binding helper bind() will always call showA() with a "this" set to obj2

setTimeout(showA.bind(obj2), 730); // 42, this time using Function.prototype.bind(), introduced in ECMAScript 5

// Function.prototype.bind() can also be used for default arguments or "partial function application"
function sum(a, b, c) {
    console.log("the sum is", a + b + c);
}
// s4() is a partial function application of sum(): s4(b, c) = sum(4, b, c)
// Note that partial function application is different than (and not a subset of, as YDKJS says) currying: if bind() would produce currying, then s4 would return another function (say s43) that would have the parameter for b fixed (say to 3). Calling s43(c) would return the sum 4+3+c.
var s4 = sum.bind(null, 4); // "this" is set to null, 'cause we don't care for it, we just use the default arguments
s4(3, 2); // 9

// Setting "this" to null can be dangerous. If you ever use that against a function call (for instance, a third-party library function that you don't control), and that function does make a "this" reference, the default binding rule means it might inadvertently reference (or worse, mutate!) the global object (window in the browser).
// A safer way to do this is to create an empty object, specifically for this purpose. It might be a good idea to name it ø (option+o on a Mac keyboard)
var ø = Object.create(null);
var saferS4 = sum.bind(ø, 4);
saferS4(3, 2); // 9


//// 4. "new" binding

function dummy() {
    this.a = 57;
}

var d = new dummy(); // "new" does the following: 1) creates a new object; 2) sets it's prototype (not used here); 3) sets "this" to point to the newly created object for the dummy() function; 4) makes dummy() return the newly created object (unless dummy() returns something else)
console.log("my dummy is", d.a);


//// Precedence of bindings

// explicit has precedence over implicit
obj.showA.call(obj2); // 42; explicit binding has precedence over implicit binding; although we called showA via obj, it uses obj2 as "this"

var dummyObj = {
    a: 5,
    dummy: dummy
};

d = new dummyObj.dummy(); // 57
console.log("'new' has precedence over implicit binding: dummyObj's a is %d, d's a is %d", dummyObj.a, d.a);

// d = new dummyObj.dummy.call(obj2); // TypeError: new and call/apply cannot be used together

var f = dummy.bind(dummyObj);
d = new f();
console.log("'new' has precedence over hard (explicit) binding: dummyObj's a is %d, d's a is %d", dummyObj.a, d.a); // 57

//// Precedence of bindings: new, explicit, implicit, default


/*****************************************************************
 * 
 * Objects
 * 
 *****************************************************************/

// There are six primitive types: number, boolean, string, null, undefined, object.
// Leaving null and undefined aside, this means that everything that's not a number, boolean or string, is an object. So a function is an object, a callable one.

// YDKJS does not define what is a JavaScript object. Since some well-known languages that have objects are OO languages (e.g. C++, Java, Python), the common assumption might be that JS is OO too. It's NOT OO, as the above discussion on "this" has pointed out. So, what's an object in JS?

// Try my own definition: a JS object is a structure that can have data and functions, and which is passed around by reference. - this is not very accurate, as an object is closer to a hash (dictionary) than to a data structure
// YDKJS mentions that the contents of an object consist of values (any type) stored at specifically named locations, which we call properties. It also mentions that: The engine stores values in implementation-dependent ways, and may very well not store them in some object container. What is stored in the container are these property names, which act as pointers (technically, references) to where the values are stored.

// So a DEFINITION: An object is a collection of named references to values of any type. These references are called properties. The values and the object itself with its references content are stored by the Engine in an implementation-dependent way. The object is passed around by reference.

// Built-in objects: Number, Boolean, String, Object, Function; Array, Date, RegExp, Error
// Although they might have the appearance of types or "classes", they are NOT.
// Instead, they are FUNCTIONS.


//// Primitive Types and their Corresponding Objects

function showTypeofAndValue(x, msg) {
    var s;
    if ( (msg == null) || (msg === undefined) ) {
        s = "x";
    } else {
        s = msg;
    }
    console.log(s, "is a", typeof x, "and has the value", x);
}
var n;

// primitive number type
n = 2;
showTypeofAndValue(n, "n"); // number 2
// Number is actually a function that returns the primitive number type
n = Number(3);
showTypeofAndValue(n, "now n"); // number 3

// Because of the "new" operator seen above, the Number() function can also be called as a constructor (but not a "class" constructor)
n = new Number(4);
showTypeofAndValue(n, "again, n"); // Number {4}
// This can be explained if we remember that "new" constructs a new object, sets its prototype (that's where Number is coming from), sets "this" to the newly created object, and calls the given function (in this case Number) making it return the new object (instead of its primitive type number)

// primitive types are not objects, but we can call methods of their corresponding object type on them. How come?
var pi = 3.1415926;
showTypeofAndValue(pi, "pi"); // it is a primitive number type
console.log("pi is a %s with the value ~ %s", typeof pi, pi.toFixed(2)); // when we call a function of the corresponding object type, the Engine coerces the primitive type to the corresponding type; it automatically creates an object of that type (in our case Number), for that call ONLY; we don't have access to that object in our code, and it is garbage collected after our function call; our variable still remains a primitive number

// console.log("pi is a %s with the string value %s", typeof pi, pi.big()); // TypeError: pi.big is not a function; so we can't call functions of an object type that does not correspond to this primitive type

(function invokedRightAway() {
    // We can, however, "hijack" the object created by the Engine when calling functions of an object type that correspond to a primitive
    // "use strict"; // but not in strict mode
    String.prototype.returnMe = function() {
        return this;
    }
    
    var sPrimitive = "abc";
    var sObject = sPrimitive.returnMe();  
    
    showTypeofAndValue(sPrimitive, "sPrimitive"); // string abc
    showTypeofAndValue(sObject, "sObject"); // object abc
})();
