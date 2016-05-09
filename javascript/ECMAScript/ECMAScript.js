/**
 * Closures
 */

function makeAdder(increment) {
    // The function "add" has access to the "increment" variable (it has a closure to that variable); this variable is accessible to "add" function even after the makeAdder function has finished
    // Sorel: I think we can think of this as a sort of reference counting: when makeAdder is invoked, it creates the "increment" variable (as a parameter) and sets its refcount to 1; when function add is created and returned, because it uses the "increment" variable, its refcount is set to 2; when makeAdder exits, its refcount is decremented, going back to 1; since it's not 0, the variable is kept and is available inside the "add" function object
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


// Demonstration that a function has a "closure" on the variables in its scope, even when those variables are gone once their scope is finished

function foo() {
    var a = 2;

    function bar() {
        console.log("I have a closure on this variable whose value is %d", a);
    }

    return bar;
}

var baz = foo();

baz(); // 2 -- Whoa, closure was just observed, man.

// Sorel: Calling bar via baz as above produces 2 because bar has a reference to variable "a". This reference is called "closure". This example and explanation is taken from the "You Don't Know JS" book, and it is consistent with my assumption of a reference-counting mechanism that allows closures to exist - Hmmm, although a few lines later the author calls this reference a "scope reference"

// console.log("I'm trying to access that variable that baz (which is a reference to bar) said to have a closure on, a = ", a); // ReferenceError, a is not declared in this scope; although baz still has access to it thanks to its closure (=reference) on it


// Another closure example from the YDKJS book - trying to print incremented numbers at 1-second intervals using setTimeout

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


// Closures used to create modules

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


/**
 * this
 */


// Quick sample about what "this" is NOT

var id = "not awesome";

(function wrongTimeout1() {
    var obj = {
        id: "awesome 1",
        cool: function coolFn() {
            console.log( this.id );
        }
    };

    obj.cool(); // awesome

    setTimeout( obj.cool, 400 ); // not awesome (because of "this" binding)
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

    obj.coolTimeout(); // not awesome (because of "this" binding)
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

    obj.coolTimeout(); // awesome 3 (because of correct "this" binding by using Function.prototype.bind)
})();


// "this" is a binding made in the activation record (execution context) of a function. The activation record is created at the call-site of the function, and makes "this" to point to the object from where the function is called. In JavaScript, everything is an object (except simple primitives - string, number, boolean, null, and undefined), including the global script (variables declared in the global scope are actually part of the global object) and functions.

// 1. default binding
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


// 2. implicit binding, when calling from a context object

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

// 3. explicit binding

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
