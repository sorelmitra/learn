# Principles & Structures

Functional programming principles | by Pradnya Borkar | Medium
https://medium.com/@kumbhar.pradnya/functional-programming-principles-6f59bc6764ff

Why Should You Care About Referential Transparency?
https://blog.rockthejvm.com/referential-transparency/

Dependency Management is an OOP-Specific Problem
https://softwareengineering.stackexchange.com/a/275969

Interfaces vs Pure Functions | Coding With Sam
https://codingwithsam.com/2019/02/22/interfaces-vs-pure-functions/

Functional Programming in TypeScript
https://www.telerik.com/blogs/functional-programming-typescript

Functional Programming in TypeScript
https://serokell.io/blog/typescript-for-haskellers

Type-Level Programming in TypeScript
https://mjj.io/2021/03/29/type-level-programming-in-typescript/

Functional TypeScript
https://blogs.halodoc.io/functional-typescript/

Memoization
Essentially you're creating a higher-level function that returns a function that has a closure on a cache of function results.  You do need to call the higher-level function only once in your program.
https://blog.openreplay.com/forever-functional-memoizing-functions-for-performance/
https://antman-does-software.com/functional-singletons-in-typescript-with-real-use-cases







# Getting a Data Structure with Optional Decorators

Say you have this `v_user` code that creates a VUser and returns it, and can also decorate created user with various field values:

## Variant with Functions - Lots of Boilerplate Code

    KEY_ID = 'id'
    KEY_ORG_ID = 'org_id'
    KEY_ROLE_ID = 'role_id'
    KEY_LAST_NAME = 'last_name'
    KEY_FIRST_NAME = 'first_name'
    KEY_PASSWORD = 'password'
    KEY_EMAIL = 'email'

    DEFAULT_LAST_NAME = 'default_last_name'
    DEFAULT_FIRST_NAME = 'default_first_name'
    DEFAULT_EMAIL_DOMAIN = "@dummy.com"
    DEFAULT_PASSWORD = get_env('V_USER_DEFAULT_PASSWORD')
    DEFAULT_MOBILE_PHONE = '123456789'

    def get_v_user(initial_user=None):
        def apply(func=None):
            user = initial_user or {
                KEY_LAST_NAME: DEFAULT_LAST_NAME,
                KEY_FIRST_NAME: DEFAULT_FIRST_NAME,
                KEY_PASSWORD: DEFAULT_PASSWORD,
                KEY_EMAIL: f"{DEFAULT_LAST_NAME}_{DEFAULT_FIRST_NAME}@{DEFAULT_EMAIL_DOMAIN}",
            }
            if func is None:
                return user
            return func(user)
        return apply


    def with_id(an_id):
        def apply_to(user):
            def with_func(func=None):
                user[KEY_ID] = an_id
                if func is None:
                    return user
                return func(user)
            return with_func
        return apply_to


    def with_org_id(org_id):
        def apply_to(user):
            def with_func(func=None):
                user[KEY_ORG_ID] = org_id
                if func is None:
                    return user
                return func(user)
            return with_func
        return apply_to


    def with_role_id(role_id):
        def apply_to(user):
            def with_func(func=None):
                user[KEY_ROLE_ID] = role_id
                if func is None:
                    return user
                return func(user)
            return with_func
        return apply_to


    def with_first_name(first_name):
        def apply_to(user):
            def with_func(func=None):
                user[KEY_FIRST_NAME] = first_name
                user[KEY_EMAIL] = f"{user[KEY_LAST_NAME]}_{first_name}@{DEFAULT_EMAIL_DOMAIN}"
                if func is None:
                    return user
                return func(user)
            return with_func
        return apply_to

Now you can create a `v_user` with pure functional programming, with optional decorators, in all these easy to read ways:

    plain_user = get_v_user()()
    # ...
    user_with_role_in_org = get_v_user()(with_org_id(org_id))(with_role_id(role_id))()
    # ...
    user = get_v_user(user_with_role_in_org)(with_first_name(get_env('user_first_name')))()
    # ...
    user = get_v_user(user)(with_id(get_env('user_id')))()

Explanation:
- `get_v_user` returns the `apply` function, which creates an initial or given user, then applies its `func` to the user and returns the result.  If the `func` is `None`, it just returns the user, thus `get_v_user()()` returns the plain user.
- The decorator functions like `with_org_id(org_id)` return an `apply_to` function, which is passable to the above `apply` function, because it takes an user.  The `apply_to` function also can apply a `func` to the user, and return either `func(user)`, or plain `user` if `func` is none.  Thus chaining can go on forever, until you pass a `func` that is `None`

## Variant with Plain Dict - Elegant 

You can, however, do this much more elegantly with plain old dict and perhaps a helper function:

    ID = 'id'
    ORG_ID = 'org_id'
    ROLE_ID = 'role_id'
    LAST_NAME = 'last_name'
    FIRST_NAME = 'first_name'
    PASSWORD = 'password'
    EMAIL = 'email'


    def get_default_v_user():
        return {
            KEY_LAST_NAME: DEFAULT_LAST_NAME,
            KEY_FIRST_NAME: DEFAULT_FIRST_NAME,
            KEY_PASSWORD: DEFAULT_PASSWORD,
            KEY_EMAIL: f"{DEFAULT_LAST_NAME}_{DEFAULT_FIRST_NAME}@{DEFAULT_EMAIL_DOMAIN}",
        }


    v_user = get_default_v_user()
    v_user[ORG_ID] = org_id
    v_user[ROLE_ID] = role_id

    def get_v_user_(user):
        return user.get()

    #...

    # Just access the prop ...
    v_user.get(ORG_ID)
    # ... maybe with a default instead of Exception ...
    v_user.get(ORG_ID, None)
    # ... or with a default function to avoid repetition
    get_v_user_prop(user)(ORG_ID)





# Writing Crystal-Clear Code


## The Problem

Assume we want to implement a helper that would simplify sending and receiving WebSockets data.  The tool has two required parameters:

- The Server.
- An Ignore List for messages sent by the Server that we want to ignore.

So we have this functional programming code:

    def ws_send_and_receive(to_send_and_receive):
        pass

        def with_server(server):
            def with_ignore_list(ignore_list):
                pass
            return with_ignore_list
        return with_server



    data = ws_send_and_receive({
      "user": "echo-user",
      "message": "Hi from the tests!"
    })(get_env('ECHO_SERVICE_URL'))(['ping', 'Hello world!'])

Calling to `ws_send_and_receive()` is not very easy to read 3 months later:

- What is the first dict?
- What is our second function expecting when we pass it an environment value?
- What is that list with `ping` containing?


## The OOP Solution

Now assume we have true object oriented code:

    class Data {
      String user;
      String message;

      public Data(user, message) { /*...*/ }

      // accessors etc...
    }

    class WebSocketsTool {
      public WebSocketsTool(server, ignore_list) {
        // initialize
      }

      public sendAndReceive(twoWayData) {
        // send and receive
      }
    }

    WebSocketsTool wst = new WebSocketsTool(
      Env.get("ECHO_SERVICE_URL"),
      ["ping", "Hello world!"]);
    wst.sendAndReceive(new Data("echo-user", "Hi from the tests!"));

This isn't much clearer, isn't it?

With OOP, you might be tempted to adapt your code to call it like this:

    // definitions...

    WebSocketsTool wst = new WebSocketsTool();
    wst.setServer(Env.get("ECHO_SERVICE_URL");
    wst.setIgnoreList(["ping", "Hello world!"]);
    wst.sendAndReceive(new Data("echo-user", "Hi from the tests!"));

Much clearer, isn't it?  But wait, an `WebSocketsTool` without a Server and an Ignore List is pretty much useless, so your constructor is not necessarily that useful.  If you forget to call the two setters, `sendAndReceive` will throw an error.  The first variant is harder to read 3 months later, but pretty common, because a constructor is supposed to build an object that's ready to use.

You probably would end up with wrapper classes like this:

    // definitions

    WebSocketsTool wst = new WebSocketsTool(
      new EchoServiceUrl(),
      new IgnoreList(["ping", "Hello world!"]);
    wst.sendAndReceive(new Data("echo-user", "Hi from the tests!"));

Which creates more classes, but perfectly easy to read and use, and pretty much flexible.


## The Functional Programming Solution

In Python, you will say:

    SERVER = 'server'
    IGNORE_LIST = 'ignoreList'

    def ws_send_and_receive(config):
        server = config.get(SERVER)
        ignore_list = config.get(IGNORE_LIST)

        def with_data(data):
            # send <data> to <server> and ignore <ignore_list>
            pass
        return with_data



    data = ws_send_and_receive(config={
      SERVER: get_env('ECHO_SERVICE_URL'),
      IGNORE_LIST: ['ping', 'Hello world!']
    })(data={
      "user": "echo-user",
      "message": "Hi from the tests!"
    })

Crystal clear and perfectly `Pythonic`.

In TypeScript:

    type Config = {
      server: string;
      ignoreList: string[];
    };

    type Data = {
      user: string;
      message: string;
    };

    const wsSendAndReceive = (config: Config) => {
      return (data: Data) => {
        // send and receive data using config
        return "a";
      }
    };

    const data = wsSendAndReceive({
      server: getEnv('ECHO_SERVICE_URL'),
      ignoreList: ['ping', 'Hello world!']
    })({
      user: "echo-user",
      message: "Hi from the tests!"
    });

    const echoSendAndReceive = wsSendAndReceive({
      server: getEnv('ECHO_SERVICE_URL'),
      ignoreList: ['ping', 'Hello world!']
    });
    echoSendAndReceive({user: "echo-user-1", message: "Hi 1"});
    echoSendAndReceive({user: "echo-user-2", message: "Hi 2"});
    echoSendAndReceive({user: "echo-user-3", message: "Hi 3"});






# Converting Object Oriented Code to Functional in JavaScript

Essentially you do the following steps:

- Make your class immutable by returning any state that would have been changed.
- Get rid of the internal Class state by adding it as parameters to methods.
- Transform the class into a plain JS object.
- Transform the JS object into a collection of functions in a file.
- Apply currying and the no-side-effects principle.

Source
https://non-traditional.dev/converting-object-oriented-code-to-functional-in-javascript-f3d50cd06d93

JavaScript is an interesting language. It doesn't fit nicely in any one Paradigm. JavaScript does have characteristics of Object-Oriented languages, but it also has attributes of Functional programming languages and doesn't strictly follow either style perfectly.

Recently I was looking at some primary computer science material and was reading about data structures, specifically, the "Stack" data structure. I noticed that all the examples tend to be object-oriented, and I wondered why other language paradigms were not shown.

That was when I decided to think about what a Stack would look like using other programming paradigms. So that is what we will try to do in this post.

(To keep the code universally readable to all readers here, I am only using JavaScript and not Typescript)

First, let's look at how I would implement a Stack using pure OO.

    class Stack {
      #head;
      push(value) {
        const node = {
          value,
          next: this.#head,
        };
        this.#head = node;
      }
      peak() {
        return this.#head?.value;
      }
      pop() {
        const safeNode = this.#head ?? {};
        const poppedValue = safeNode.value;
        this.#head = safeNode.next;
        return poppedValue;
      }
      [Symbol.iterator] = function* () {
        let safeNode = this.#head ?? {};
        while (safeNode.value) {
          yield safeNode.value;
          safeNode = safeNode.next ?? {};
        }
      };
    }

    const stack = new Stack();
    stack.push(1);
    stack.push(2);
    stack.peak(); // 2
    stack.pop(); // 2
    stack.peak(); // 1
    stack.push(3);
    stack.push(4);
    stack.push(2);
    stack.push(2);
    console.log([...stack]); // [ 2, 2, 4, 3, 1 ]

In the above code, we are defining a Stack class that has a private property called #head and three methods called push , pop , and peak that allows you to add an item to the stack, take an item off, and look at the top item on the stack.
Our Stack is implemented using a linked list. If you are unfamiliar with linked lists, I recommend checking out the basecs article on linked lists but summarizing: A linked list is a list of objects called nodes. Each node has two properties: a value and a reference to the next node. To traverse a linked list, you start at the first node, called the head, and keep going to the next node until you run out of nodes.

Our Stack keeps track of the head node in the private #head property. When we call peak it returns the value at the head node. When we call push, it will create a new node with the value provided, assign the node at the #head property to next and assign that new node to the#head property. Finally, when we call pop, we pull the value off the#head property, and then assign the value of next to the#head property.

We are also making our Stack iterable by assigning a generator function to the[Symbol.iterator] property. You can learn more about generator functions in my blog post on them, but simply put, this allows JavaScript to iterate over the items in the stack, and we can do things like spreading them into an array or using them in a for...of loop.

To use the stack, we call new Stack() and assign that to a variable. From there, we can do all the fun things to a stack you can. This is probably the most common way you will see a Stack implemented.

The first step to making this more functional is to stop mutating our Stack every time we call either the push or pop method. So let's make a new version called ImmutableStack:

    class ImmutableStack {
      #head;

      constructor(node) {
        this.#head = node;
      }

      push(value) {
        const node = {
          value,
          next: this.#head,
        };

        return new ImmutableStack(node);
      }

      peak() {
        return this.#head?.value;
      }

      pop() {
        const safeNode = this.#head ?? {};

        return new ImmutableStack(safeNode.next);
      }

      [Symbol.iterator] = function* () {
        let safeNode = this.#head ?? {};
        while (safeNode.value) {
          yield safeNode.value;
          safeNode = safeNode.next ?? {};
        }
      };
    }

    let stack = new ImmutableStack();

    stack = stack.push(1);
    stack = stack.push(2);
    stack.peak(); // 2
    stack = stack.pop();
    stack.peak(); // 1

    stack = stack.push(3);
    stack = stack.push(4);
    stack = stack.push(2);
    stack = stack.push(2);

    console.log([...stack]); // [ 2, 2, 4, 3, 1 ]

Let's talk about what we changed. We still have a private #head property. Also, our peak and iterator or also still the same. What we changed was that we added a constructor to our class. The constructor allows us to initialize values in our Stack when we call new Stack() in our code. Our constructor lets us optionally pass in a node and assign it to the#head property. This will allow us to initialize our Stack with an existing linked list if we want.

In our push method, we still take in a value, assign it to a new node, and assign the value at the #head property to the next property of that new node. However, instead of changing the value of the #head property, we are returning a new Stack initialized with the new node. The same thing happens in our pop method. We are returning a new Stack initialized with the next node of our stack.

I think it is essential to stop and reiterate why this is helpful. If two parts of our code were looking at that same Stack and one part called push or pop on it. The other part of our code would not know that the value was changed on it. This can lead to unpredictable behavior. Making our Stack immutable ensures that we can write our code and be confident that some other part of our code won't change its values underneath us.

The next evolution into a more functional style is to move away from creating classes at all and instead define methods that take inputs and return values:

    const stackUtils = {
      push(head, value) {
        const node = {
          value,
          next: head,
        };

        return node;
      },

      peak(head) {
        return head?.value;
      },

      pop(head) {
        const safeNode = head ?? {};

        return safeNode.next;
      },

      getIterator(head) {
        return (function* () {
          let safeNode = head ?? {};
          while (safeNode.value) {
            yield safeNode.value;
            safeNode = safeNode.next ?? {};
          }
        })();
      },
    };

    let stack;

    stack = stackUtils.push(stack, 1);
    stack = stackUtils.push(stack, 2);
    stackUtils.peak(stack); // 2
    stack = stackUtils.pop(stack);
    stackUtils.peak(stack); // 1

    stack = stackUtils.push(stack, 3);
    stack = stackUtils.push(stack, 4);
    stack = stackUtils.push(stack, 2);
    stack = stackUtils.push(stack, 2);

    console.log([...stackUtils.getIterator(stack)]); // [ 2,2,4,3,1 ]

In the above code, instead of creating a class, we define an object with utility methods that know how to peak, push, and pop values on a linked list. Since our methods no longer have access to a head property, that value must be passed in as an argument to our methods. For that same reason, we also have created a function getIterator that will generate an iterator for us to use in those circumstances.

Other than that, our code works pretty much the same way as our ImmutableStack. The only difference is that we must pass the stack variable into every method call.

The above example already follows a functional style, but we can go to one more level of inception. So let's show you that code first:

    //stackUtils.js

    export function peak(head) {
      return head?.value;
    }

    export function pop(head) {
      const safeNode = head ?? {};

      return safeNode.next;
    }

    export function push(head) {
      return (value) => {
        const node = {
          value,
          next: head,
        };
        return node;
      };
    }

    export function getIterator(head) {
      return (function* () {
        let safeNode = head ?? {};
        while (safeNode.value) {
          yield safeNode.value;
          safeNode = safeNode.next ?? {};
        }
      })();
    }

    // index.js

    import { push, pop, peak, getIterator } from "./stackUtils.js";

    let stack;

    stack = push(stack)(1);
    stack = push(stack)(2);
    peak(stack); // 2
    stack = pop(stack);
    peak(stack); // 1

    stack = push(stack)(3);
    stack = push(stack)(4);
    stack = push(stack)(2);
    let stackPusher = push(stack);

    stack = stackPusher(2);

    console.log([...getIterator(stack)]); // [ 2, 2, 4, 3, 1 ]

First, it is more common in FP languages to group related functions in a module, so that was the first thing we did. We move all the methods into simple functions that we export from thestackUtils.js file.

Generally speaking, all the functions work the same way except for the push function. A common functional programming pattern is only one parameter per function. If you need more than one argument, you use a pattern called "currying." When you curry functions, your function will return a new function that requires the next argument. You keep doing this until you no longer need any arguments.

In our push function. We take an argument called head and return a new function that takes an argument called value. When we call that second function, it will still have access to that head value and can complete the code we need to run.

One of the things this gives us is the ability to do "partial application." On one of the lines near the end, we create a new function stackPusher, that will take a value and push it onto a new stack.

This gives us two things we can do. We may know which stack we want to push onto in one part of our code, but we don't yet know the value. Instead of trying to keep track of the stack in some global store, we can pass around a function that already knows which stack to use and needs to have a value passed in:

    const stackPusher = push(stack)

    /* later in our code */

    const newStack = stackPusher(5)

This can be helpful if we want to create multiple stacks with the same tail but with different heads. We can create a function from the push function that we can use to generate multiple lists with the same tail using different values:

    const stackPusher = push(stack)

    const stack1 = stackPusher(5)  
    const stack2 = stackPusher(2)  
    const stack3 = stackPusher(3)  
    const stack4 = stackPusher(8)  
    const stack5 = stackPusher(12)

So there are four styles of code that you can use to do the same thing. One is a typical OO style, and the last is a distinctive FP style with all the variations in between. I recommend trying all these styles out and seeing which makes the most sense.








# Software Design with Functional Programming

**Question**: << I've recently forayed into the world of functional programming (FP) and am wondering how to "think functionally" for even moderately sized applications? Especially w.r.t. the analysis and design of FPs.

With OOP we're trained to think in terms of objects, their attributes and relations. We model our analyses/designs using class and sequence diagrams. However, the same models seem to be a bad fit when designing for FPs. What are the equivalent modeling paradigms for functional programming? It seems DFDs maybe a good fit but I maybe wrong.

For example: I was thinking of designing a simulation of Monopoly, the board game using Haskell, just to learn the language. When doing OOAD you come up with classes like board contains items that have attributes/methods attached to it. You have player and various other objects and their associated relations that can be captured in a class diagram. And their interactions in a sequence diagram. However, these modeling paradigms doesn't seem to transfer well for functional programs. So just "how" do you model functionally? >>


**Answer 1**: << According to Simon Peyton Jones:

The language in which you write profoundly affects the design of programs written in that language. For example, in the OO world, many people use UML to sketch a design. In Haskell or ML, one writes type signatures instead. Much of the initial design phase of a functional program consists of writing type definitions. Unlike UML, though, all this design is incorporated in the final product, and is machine-checked throughout.
Source: Masterminds of Programming

So instead of drawing all the fancy UML diagrams, you actually write type definitions coupled with undefined in the design phase. >>

**Answer 2**: << All of my programming these days consists of single-person projects. If I were collaborating on a project with other programmers, I think that writing type definitions and using undefined would be a good approach.

But I gather what you're really looking for is some advice about how you can learn to think functionally. So here are some thoughts.

When programming in Haskell, there are two ways I think about the program I'm writing.

If the program is mathematical, I think of the program as a set of equations.

Otherwise, I tend to think of the program as one or more chains of of data transformations. (So perhaps DFDs would be useful.)
So in your Monopoly example, my first thought would be to figure out how I'm going to represent the state of the board (e.g., which properties have houses, who owns them). Then I might have a function that transforms the board when someone buys a property, and other functions for other things players might do. (There's also monads for representing state, State and StateT. I might use them, if and when I feel they will make the code clearer, but I usually keep things basic to start.)

One of the mistakes I made most often as a beginner was to create a lot of unnecessary classes and data types. >>

Source: https://stackoverflow.com/questions/22605097/as-ooad-is-to-oop-what-is-the-equivalent-for-functional-programming

