//
//  main.m
//  ObjCFoundationLearn
//
//  Created by Sorel Mitra on 3/26/14.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import <Foundation/Foundation.h>


// Type C pointer to a function
typedef void (*PRINTFUNC)(NSString *format, ...);


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class

/*
Objective-C's classes have two parts: a declaration, called "interface",
and a implementation.
*/

/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class interface

@interface Tree: NSObject
/*
Instance variables are inside accolades immediately after @interface.
They are protected; you cannot access them ouside the class.
Thus, they are most useful to hold internal data during the object's 
lifetime.
For publicly available data you use @property declarations.
*/
{
    id value;
    NSMutableArray *children;
}

- (id)init;
- (Tree *)addChildToRight:(NSString *)value;

/*
Method with two parameters.
The name of this method is: "printPreOrderWithFunc: andLevel:"
The first word "printPreOrderWithFunc" has three parts:
- printPreOrder: name of the action that the method performs
- With: joining word
- Func: name of the first parameter; the actual parameter variable is called
  "prt"
Similarly, the second word "andLevel" has two parts:
- And: joining word
- Level: the name of the second parameter; the actual parameter variable
  is called "level".

The first parameter, "Func" is actually a C pointer to a function, which
works perfectly in Objective-C.
*/
- (void)printPreOrderWithFunc:(PRINTFUNC)prt andLevel:(int)level;

/*
A property is accessible from outside the class, and is actually a function.
Unless given an explicit @synthesize declaration the compiler will
automatically synthesize getter, setter, and instance variable for it.
The instance variable will be _value.

If you have declared a "value" instance variable you will get a warning that
it will not be linked to the "value" property".
There are two solutions: either specifically link the property to a 
instance variable (shown below at @synthesize value) either rename
the instance variable.
*/
@property (strong) id value;

/*
These two properties share the same type, so they are on the same line,
separated by comma.
*/
@property (readonly) int x, y;

@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class Extension

/*
A class extension bears some similarity to a category, but it can only 
be added to a class for which you have the source code at compile time 
(the class is compiled at the same time as the class extension). 
The methods declared by a class extension are implemented in the 
@implementation block for the original class so you can’t, for example, 
declare a class extension on a framework class, such as a Cocoa or 
Cocoa Touch class like NSString.

The syntax to declare a class extension is similar to the syntax for 
a category, but you don't put a name inside the parentheses.

You cannot put the class extension after the implementation body (like you
can do with class categories).

The main usage of a class extension is to redefine a read-only property,
declared in the interface file, as read-write in a class extension block that
is placed in the implementation file before the @implementation block. 
This way the class user sees the property as read-only while the class
implementation sees it as read-write.
*/
@interface Tree () // Class extension for Tree (anonymous category)
- (void)doStuff2; // not very useful, could be placed in the main interface

@property (readwrite) int x, y; // redeclare them as read-write
@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class implementation

@implementation Tree

/*
This links the "value" property to the "value" instance variable, instead
of the automatically synthesized "_value".
Note that both the property and instance variable can have the same name.
*/
@synthesize value = value;

- (id)init
{
    self = [super init];
    if (self) {
        children = [NSMutableArray arrayWithCapacity:10];
    }
    return self;
}

- (Tree *)addChildToRight:(NSString *)val
{
    Tree *child = [[Tree alloc] init];
    [child setValue:val];
    [children addObject:child];
    return child;
}

- (void)printPreOrderWithFunc:(PRINTFUNC)prt andLevel:(int)level
{
    Tree *t = self;
    NSMutableString *indent = [NSMutableString stringWithString:@""];
    for (int i = 0; i < level; i++) {
        [indent appendString:@"    "];
    }
    prt(@"%@%@", indent, [t value]);
    
    // Enumerating objects of a collection
    // "children" is a instance variable, not a property
    NSEnumerator *en = [children objectEnumerator];
    while ((t = [en nextObject])) {
        // Recursivity
        [t printPreOrderWithFunc:prt andLevel:(level+1)];
    }
}

/*
This is from the Tree class extension
*/
- (void)doStuff2
{
    NSLog(@"%@ doing stuff 2", self);
}
@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class Category


/*
Between braces after an interface/implementation declaration you specify
a "category". This simply adds functionality to an existing class, whose
source code you don't necessary have.
The methods declared here can be called on an original Tree instance.
*/
@interface Tree (SomeCategory)
- (void)doStuff;
/*
If the name of a method declared in a category is the same as a method 
in the original class, or a method in another category on the same class 
(or even a superclass), the behavior is undefined as to which method 
implementation is used at runtime.
*/
@end

@implementation Tree (SomeCategory)
- (void)doStuff
{
    // We have "self" accesible here
    NSLog(@"%@ doing stuff", self);
}
@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Protocol

/*
An Objective-C protocol is like what other OO languages call "interface"
or "pure abstract class": it's a specification of an methods to be
implemented by all classes that claim to conform to this protocol.
*/
@protocol Serializable
- (bool)serializeToFile:(NSString *)file;
- (id)deSerializeFromFile:(NSString *)file;
@end

@protocol Stringizeable
- (NSString *)convertToString;
@end

/*
The Drawable protocol "inherits" from the Stringizeable protocol, that is
it also provides the methods of the Stringizeable protocol.
*/
@protocol Drawable <Stringizeable>
- (void)drawToCanvas:(id)canvas;
- (void)eraseFromCanvas:(id)canvas;
@end

/*
Uncomment this line to get a warning "defined without specifying a
base class"
*/
// @interface DataStuff <Serializable>

/*
This class declares that it implements the "Serializable" protocol
*/
@interface DataStuff: NSObject <Serializable>
@end

/*
Here we must implement the methods declared in Serializable
*/
@implementation DataStuff
- (bool)serializeToFile:(NSString *)file
{
    return false;
}

- (id)deSerializeFromFile:(NSString *)file
{
    return nil;
}
@end


/*
Now another class, conforming to two protocols
*/
@interface FooObject: NSObject <Serializable, Drawable>
@end

/*
Here we must implement the methods declared in Serializable, Drawable,
and, by protocol inheritance, Stringizeable
*/
@implementation FooObject
- (bool)serializeToFile:(NSString *)file
{
    return false;
}

- (id)deSerializeFromFile:(NSString *)file
{
    return nil;
}

- (NSString *)convertToString
{
    return [NSString stringWithFormat:@"%@", self];
}

- (void)drawToCanvas:(id)canvas
{
    NSLog(@"drawing %@ to canvas %@", self, canvas);
}

- (void)eraseFromCanvas:(id)canvas
{
    NSLog(@"erasing %@ from canvas %@", self, canvas);
}
@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// variadic methods

/*
Keep in mind that the implementation of an Objective-C method is just
a block of code, like a C function. The variadic argument macros 
described in the stdarg(3) manual page work the same way in a method 
as they do in an ordinary function.
*/

@interface ClassWithVariadicMethod: NSObject
/*
You need to specify a first argument before the C ellipsis.
This function expects id values after count.
*/
- (void)printArguments:(int)count, ...;
@end

@implementation ClassWithVariadicMethod
- (void)printArguments:(int)count, ...
{
    NSLog(@"My arguments are:");
    
    // The remaining of arguments are handled like in C
    va_list ap;
    va_start(ap, count);
    id arg;
    for (int i = 0; i < count; i++) {
        arg = va_arg(ap, id);
        NSLog(@"%d - %@", i+1, arg);
    }
    va_end(ap);
}
@end


/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// Class with block callback

@interface ClassWithBlockCallback: NSObject
- (void)computeWithCallbackBlock:(void (^)(int))callbackBlock;
@end

@implementation ClassWithBlockCallback
- (void)computeWithCallbackBlock:(void (^)(int))callbackBlock
{
    int result = 5*6;
    callbackBlock(result);
}
@end

/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
//////// main() function

/*
This is a Command Line application that uses the Objective-C Foundation
library.
It has the typical C main() function
*/
int main(int argc, const char * argv[])
{
    // ObjC Foundation classes and functions usually begin with NS
    NSLog(@"Hello, World!");
    
    // Declare, allocate, and initialize a class
    Tree *t = [[Tree alloc] init];
    
    // Cannot access class instance variable; uncomment to get error
    // t->value = @"leaf";
    
    // Accesing the property (which is a method) instead
    [t setValue:@"root"];
    
    // Method call
    Tree *t1 = [t addChildToRight:@"node 1"];
    
    // Multiple method call: add a tree node, then add another tree
    // node to the newly added one
    [[t addChildToRight:@"node 2"] addChildToRight:@"node 2-1"];
    
    // Call function with two parameters
    // The first parameter is the address of a C function that
    // "printPreOrderWithFunc:andLevel:" calls in order to print on the screen
    [t printPreOrderWithFunc:&NSLog andLevel:0];

    // Print an Objective-C object with "%@"
    NSLog(@"My tree object is %@ with value '%@'", t, [t value]);
    
    // This method was added in category "SomeCategory" and is not
    // part of the original Tree interface
    [t doStuff];

    // This method was added in the class extension (anonymous category)
    // and is not part of the original Tree interface
    [t doStuff2];
    
    // This call would fail if the Tree @implementation block would be
    // placed in its separate file along with the class extension
    [t setX:5];
    
    // To access a class via the protocol(s) it conforms to, you use
    // type "id", and a syntax similar to the protocol declaration
    id <Drawable> drawMe = [[FooObject alloc] init];
    [drawMe drawToCanvas:nil];

    // Print some integers
    NSLog(@" here are some integers: %d %i", 1, 2);
    
    // Variadic method - method with a variable number of arguments,
    // with the same syntax as a C function with variable number of
    // arguments
    ClassWithVariadicMethod *vm = [[ClassWithVariadicMethod alloc] init];
    // When calling a variadic method, you can only put arguments of the type
    // expected by the caller.
    // In this case you can't put scalar types (e.g. int, char *, etc.)
    [vm printArguments: 5, @"first argument", drawMe, @"third argument", nil,
        @"fifth argument"];
    
    /////////////////////////////
    // Blocks (Clojures, Lambdas)
    
    // Simple block with no return values or arguments
    // How do you call it w/o a variable? The compiler also gives a warning here
    ^ {
        NSLog(@"This a block 1");
    };

    // Block variable - similar syntax to a C pointer to function
    double (^multiplyTwoValues) (double firstValue, double secondValue);
    
    int variableOutsideBlock = 10;
    // Assign block variable to a block with the same "signature"
    multiplyTwoValues = ^ double (double firstValue, double secondValue) {
        // Blocks can *capture* a variable defined outside it
        // The variable is captured with the value it had at the moment
        // of the block instantiation.
        return firstValue * secondValue + variableOutsideBlock;
    };
    
    // Incrementing the variable, but the block will use its previously captured
    // value
    variableOutsideBlock++;
    
    // Call block via variable
    double x = multiplyTwoValues(3, 6.8);
    NSLog(@"x is %f", x);

    // Give block as a parameter to a method (callback)
    ClassWithBlockCallback *computator = [[ClassWithBlockCallback alloc] init];
    [computator computeWithCallbackBlock:^ (int result) {
        NSLog(@"result of computation with callback block is %d", result);
    }];
    
    // TODO:
    // Exception handling is similar to C++ (@try, @catch, @finally), but you
    // should use exceptions for programmer errors only (out-of-bounds, invalid
    // method arguments).
    // For errors related to the user of the app (no network connectivity, bad
    // input data, etc) you should use NSError.
    
    return 0;
}

