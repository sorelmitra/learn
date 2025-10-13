Update: 2021-03-12
Latest Java version used: 11

# Language Features

## Generics

### Generics Type Naming Conventions

- E – Element (used extensively by the Java Collections Framework, for example ArrayList, Set etc.)
- K – Key (Used in Map)
- N – Number
- T – Type
- V – Value (Used in Map)
- S,U,V etc. – 2nd, 3rd, 4th types

### Generics Class

	public class GenericsType<T> {
		private T t;
		
		public T get() { return this.t; }
		public void set(T t1) { this.t = t1; }
	
		public static void main(String args[]){
			GenericsType<String> type = new GenericsType<>();
			type.set("String me"); //valid
		
			GenericsType type1 = new GenericsType(); //raw type
			type1.set("String me again"); //valid
			type1.set(10); //valid and autoboxing support
		}
	}

### Generics Interface

	public interface Comparable<T> {
		public int compareTo(T o);
	}

### Generics Method

	public class GenericsMethods {
		//Java Generic Method
		public static <T> boolean isEqual(GenericsType<T> g1, GenericsType<T> g2) {
			return g1.get().equals(g2.get());
		}
	
		public static void main(String args[]){
			GenericsType<String> g1 = new GenericsType<>();
			g1.set("String me");
			GenericsType<String> g2 = new GenericsType<>();
			g2.set("String me");
		
			boolean isEqual = GenericsMethods.<String>isEqual(g1, g2);
			
			//The above statement can be written simply as below
			//This feature, known as type inference, allows you to invoke a generic method as an ordinary method, without specifying a type between angle brackets.
			//Compiler will infer the type that is needed
			isEqual = GenericsMethods.isEqual(g1, g2);
		}
	}

### Generics Bounded Type Parameter

	public static <T extends Comparable<T>> int compare(T t1, T t2){
		return t1.compareTo(t2);
	}

### Generics Sub-typing

	interface MyList<E,T> extends List<E>{
	}

### Generics Diamond `<>` Operator

Reduce verbosity, rather than:

	Map<String, Set<String>> rowsMap = new HashMap<String, Set<String>>();

Write:

	Map<String, Set<String>> rowsMap = new HashMap<>();

### Generics Upper Bounded Wildcard

	public static double sum(List<? extends Number> list){
		double sum = 0;
		for (Number n : list){
			sum += n.doubleValue();
		}
		return sum;
	}
	

`?` can be any subclass of `Number`.

### Generics Lower Bounded Wildcard

	public static void addIntegers(List<? super Integer> list){
		list.add(new Integer(50));
	}

`?` can be any superclass of `Integer`.

### Generics Unbounded Wildcard

	public static void printData(List<?> list){
		for(Object obj : list){
			System.out.print(obj + "::");
		}
	}


### Generics Type Erasure

This code:

	List<String> list = new ArrayList<String>();
	list.add("Hi");
	String x = list.get(0);

is compiled into

	List list = new ArrayList();
	list.add("Hi");
	String x = (String) list.get(0);

The compiler infers what's the best code to compile Generics code into, and throws away Generics info at compile time.


## Annotations

### Custom Annotation

	import java.lang.annotation.Documented;
	import java.lang.annotation.ElementType;
	import java.lang.annotation.Inherited;
	import java.lang.annotation.Retention;
	import java.lang.annotation.RetentionPolicy;
	import java.lang.annotation.Target;
	import java.lang.annotation.Repeatable;

	@Repeatable(Schedules.class)
	public @interface Schedule {
	  String dayOfMonth() default "first";
	  String dayOfWeek() default "Mon";
	  int hour() default 12;
	}
	
	public @interface Schedules {
		Schedule[] value();
	}
	
	@Documented
	@Target(ElementType.METHOD)
	@Inherited
	@Retention(RetentionPolicy.RUNTIME)
	@Schedule(dayOfMonth="last")
	@Schedule(dayOfWeek="Fri", hour="23")
	public @interface MethodInfo{
		String author() default "John Doe";
		String date();
		int revision() default 1;
		String comments();
	}
	
	public class X {
		@MethodInfo(author = "Trevor", date = "April")
		public void method() {
			x = 1;
		}
	}

- Annotation methods can’t have parameters.
- Annotation methods return types are limited to primitives, String, Enums, Annotation or array of these.
- Java Annotation methods can have default values.
- Annotations can have meta annotations attached to them, which provide information about the annotation.  There are five types of meta annotations:
	- `@Documented` – use it to ensure that things using your annotation show this in their generated JavaDoc.
	- `@Target` – where annotation is applicable, such as TYPE, METHOD, CONSTRUCTOR, FIELD etc. If missing, then annotation can be used anywhere.
	- `@Inherited` – annotation is inherited in subclasses.
	- `@Retention` – where the annotation is retained: SOURCE (discard after compiling), CLASS (discard after class loading) and RUNTIME (not discarded).
	- `@Repeatable` – the annotation can be repeated on the same element, as is `Schedule` above.


## Reflection

Set private field's value via reflection (I know, not best practice!):

	public class TestHelperAccessor {
		public static void set(Object object, String fieldName, Object fieldValue) {
			try {
				Field field = object.getClass().getDeclaredField(fieldName);
				field.setAccessible(true);
				field.set(object, fieldValue);
			} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		}
	}


## Enum

	public enum Planet {
		MERCURY (3.303e+23, 2.4397e6),
		VENUS   (4.869e+24, 6.0518e6),
		EARTH   (5.976e+24, 6.37814e6),
		MARS	(6.421e+23, 3.3972e6),
		JUPITER (1.9e+27,   7.1492e7),
		SATURN  (5.688e+26, 6.0268e7),
		URANUS  (8.686e+25, 2.5559e7),
		NEPTUNE (1.024e+26, 2.4746e7);
		
		private final double mass;   // in kilograms
		private final double radius; // in meters
		
		Planet(double mass, double radius) {
			this.mass = mass;
			this.radius = radius;
		}
		
		private double mass() { return mass; }
		private double radius() { return radius; }
		
		// universal gravitational constant  (m3 kg-1 s-2)
		public static final double G = 6.67300E-11;
		
		double surfaceGravity() {
			return G * mass / (radius * radius);
		}
		double surfaceWeight(double otherMass) {
			return otherMass * surfaceGravity();
		}
		
		public static void main(String[] args) {
			if (args.length != 1) {
				System.err.println("Usage: java Planet <earth_weight>");
				System.exit(-1);
			}
			double earthWeight = Double.parseDouble(args[0]);
			double mass = earthWeight/EARTH.surfaceGravity();
			for (Planet p : Planet.values())
			   System.out.printf("Your weight on %s is %f%n",
								 p, p.surfaceWeight(mass));
		}
	}

So `Enum` is a full-fledged class but used in a specific and constrained way to provide constan-like behavior.


## Varargs

	static void fun2(String str, int ...a)
	{
		System.out.println("String: " + str);
		System.out.println("Number of arguments is: "+ a.length);
		
		// using for each loop to display contents of a 
		for (int i: a) {
			System.out.print(i + " ");
		}
		
		System.out.println();
	}


## Instrumentation (`premain` method)

	import java.lang.instrument.*;
	import com.mypackage.SomeClass;

	public class MyAgent {
		public static void premain(String args, Instrumentation inst) {
			SomeClass obj = new SomeClass();
			long size = inst.getObjectSize(obj);
			System.out.println("Bytes used by object: " + size);
		}
	}


## Strings in Switch

	switch (country) {
		case "UK":
		case "US":
			System.out.printlin("Hi!");
			break;
		case "DE":
			System.out.printlin("Hallo!");
			break;
		default:
			System.out.printlin("Hello!");
	}


## Try with Resource

If a resource implements java.lang.AutoCloseable, which includes all objects implementing java.io.Closeable, will be auto closed if they are created within try statement:

	File file = new File("somefile.txt");
	
	try (FileInputStream fis = new FileInputStream(file)) {
	   System.out.println("Total file size (in bytes) : "
		  + fis.available());
		  
	   int content;
	   while ((content = fis.read()) != -1) {
		  System.out.print((char) content);
	   }
	   
	} catch (IOException e) {
		e.printStackTrace();
	}


## Lambda Expressions

	// simple Lambda function to add two ints
	(int x, int y) -> x + y
	
	// lambda expression used as a comparator
	Comparator<Apple> byWeight = 
		(Apple a1, Apple a2) -> a1.getWeight().compareTo(a2.getWeight());
		
	// Defining Runnable by Lambda expression
	Runnable java8Runner = () -> {
		System.out.println("I am running");
	};


## Functional Interfaces

Declare an interface that accept a Lambda Expression:

	@FunctionalInterface // Can be ommitted if there's only one method in the interface
	public interface Predicate<T> {
		boolean test(T t);
	}


## Method References

	class ComparisonProvider {
		public int compareByName(Person a, Person b) {
			return a.getName().compareTo(b.getName());
		}
		
		public int compareByAge(Person a, Person b) {
			return a.getBirthday().compareTo(b.getBirthday());
		}
	}
	ComparisonProvider myComparisonProvider = new ComparisonProvider();
	Arrays.sort(personsArray, myComparisonProvider::compareByName);

- Reference to a static method: `ContainingClass::staticMethodName`
- Reference to an instance method of a particular object: `containingObject::instanceMethodName`
- Reference to an instance method of an arbitrary object of a particular type: `ContainingType::methodName`
- Reference to a constructor: `ClassName::new`


## Streams API

A stream is a iterator that allows a single run over the collection it is called on. Streams can run sequentially or in parallel as desired. The parallel mode makes use of fork/join framework and can leverage power of multiple cores. Check java.util.stream.Stream API for more details.

	Collection<Widget> widgets = ...

	int sum = widgets.stream()
		.filter(w -> w.getColor() == RED)
		.mapToInt(w -> w.getWeight())
		.sum();


## Optional

If we know that `doSomething()` may not return a desired Foo object, we can change do:

	public Optional<Foo> doSomething();

Optional provides a suite of methods—many functional—that allows a client to decide what to do when the desired value is not present. For example, we can use the orElse method to return a default value when the desired value cannot be found (called an empty Optional in the Optional lexicon):

	Foo foo = doSomething()
		.orElse(new Foo());

Likewise, we can also throw an exception when Optional is empty using the orElseThrow method:

	Foo foo = doSomething()
		.orElseThrow(SomeException::new);


## Var Locals

Since Java 10 you can do this:

	var greeting = "Hello World";
	var messages = new ArrayList<String>();
	messages.add(greeting);
	var stream = messages.stream();

Where you can use it:

- local variables with initializers
- indexes in for each loops
- locals in for loops

Where you cannot use it:

- method formals
- constructor formals
- method return types
- fields
- catch formals

Basically, the only time you can use var is when initialising local variables and in for loops.

It helps reduce boilerplate code, but makes code less readable, as variable types are not always obvious.

## Multi-Release JARs

Multi-release JARs allow you to create a single JAR that contains bytecode for several Java versions with jar --version 9 (or 10, or...). Presented with a multi-release JAR, JVMs version 9 and later will load the code that was included for their version.

	# compile code in `src/main/java` for Java 8 into `classes`
	$ javac --release 8
		-d classes
		src/main/java/org/codefx/detect/*.java
		
	# compile code in `src/main/java-9` for Java 9 into `classes-9`
	$ javac --release 9
		-d classes-9
		src/main/java-9/module-info.java
		src/main/java-9/org/codefx/detect/DetectVersion.java
		
	# when packaging the bytecode into a JAR, the first part (up to
	# `-C classes .`) packages "default" bytecode from `classes` as
	# usual; the new bit is the `--release 9` option, followed by
	# more classes to include specifically for Java 9
	$ jar --create
		--file target/detect.jar
		-C classes .
		--release 9
		-C classes-9 .


## Modules

https://beginnersbook.com/2018/09/java-9-modules/

A module is a set of packages. We have two types of packages in a module – 1) Exported packages 2) Concealed packages.

Exported Packages: These packages are intended to be used outside of the module, which means any program residing in any other module can use these packages.

Concealed Packages: These packages are not intended to be used outside, they are internal to the module and can be used inside the module only.

https://www.baeldung.com/java-9-modularity

List System modules:

	java --list-modules

There are four types of modules in the module system:

- System Modules – These are the modules listed when we run the `list-modules` command above. They include the Java SE and JDK modules.
- Application Modules – These modules are what we usually want to build when we decide to use Modules. They are named and defined in the compiled module-info.class file included in the assembled JAR.
- Automatic Modules – We can include unofficial modules by adding existing JAR files to the module path. The name of the module will be derived from the name of the JAR. Automatic modules will have full read access to every other module loaded by the path.
- Unnamed Module – When a class or JAR is loaded onto the classpath, but not the module path, it's automatically added to the unnamed module. It's a catch-all module to maintain backward compatibility with previously-written Java code.

### Requires

	module my.module {
		requires module.name;
	}

Now, my.module has both a runtime and a compile-time dependency on module.name.

### Requires Static

Sometimes we write code that references another module, but that users of our library will never want to use.  In these cases, we can use the requires static directive, to create a compile-time-only dependency:

	module my.module {
		requires static module.name;
	}

### Requires Transitive

We need to make sure that any module that brings in our code will also bring in these extra ‘transitive' dependencies or they won't work.  So we do this:

	module my.module {
		requires transitive module.name;
	}

Now, when a developer requires `my.module`, they won't also have also to say requires `module.name` for our module to still work.

### Exports

By default, a module doesn't expose any of its API to other modules. This strong encapsulation was one of the key motivators for creating the module system in the first place.

Our code is significantly more secure, but now we need to explicitly open our API up to the world if we want it to be usable.  We use the exports directive to expose all public members of the named package:

	module my.module {
		exports com.my.package.name;
	}

Now, when someone does requires my.module, they will have access to the public types in our com.my.package.name package, but not any other package.

### Uses

	module my.module {
		uses fully.qualified.class.name;
	}

We should note here that there's a difference between a requires directive and the uses directive.

We might require a module that provides a service we want to consume, but that service implements an interface from one of its transitive dependencies.

Instead of forcing our module to require all transitive dependencies just in case, we use the uses directive to add the required interface to the module path.

### Provides … With

A module can also be a service provider that other modules can consume.

The first part of the directive is the provides keyword. Here is where we put the interface or abstract class name.  Next, we have the with directive where we provide the implementation class name that either implements the interface or extends the abstract class.

	module my.module {
		provides MyInterface with MyInterfaceImpl;
	}

### Open

To explicitly grant permission for other modules to reflect on all classes in this module:

	open module my.module {
	}

### Opens

To explicitly grant permission for other modules to reflect on a particular package in this module:

	module my.module {
		opens com.my.package;
	}


## Future

https://www.baeldung.com/java-future

### Definition

Approximation of the interface definition:

	public interface Future<V> {
		boolean cancel(boolean mayInterruptIfRunning)
		V	   get();
		V	   get(long timeout, TimeUnit unit);
		boolean isCancelled();
		boolean isDone();
	}

### Implement

	public class SquareCalculator {	
		
		private ExecutorService executor = Executors.newSingleThreadExecutor();
		
		public Future<Integer> calculate(Integer input) {		
			return executor.submit(() -> {
				Thread.sleep(1000);
				return input * input;
			});
		}
	}

### Consume

	Future<Integer> future = new SquareCalculator().calculate(10);
	
	while(!future.isDone()) {
		System.out.println("Calculating...");
		Thread.sleep(300);
	}
	
	Integer result = future.get();


## ExecutorService

Some tasks:

	Runnable runnableTask = () -> {
		try {
			TimeUnit.MILLISECONDS.sleep(300);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	};

	Callable<String> callableTask = () -> {
		TimeUnit.MILLISECONDS.sleep(300);
		return "Task's execution";
	};

	List<Callable<String>> callableTasks = new ArrayList<>();
	callableTasks.add(callableTask);
	callableTasks.add(callableTask);
	callableTasks.add(callableTask);

The execute() method is void and doesn't give any possibility to get the result of a task's execution or to check the task's status (is it running):

	executorService.execute(runnableTask);

submit() submits a Callable or a Runnable task to an ExecutorService and returns a result of type Future:

	Future<String> future = executorService.submit(callableTask);

invokeAny() assigns a collection of tasks to an ExecutorService, causing each to run, and returns the result of a successful execution of one task (if there was a successful execution):

	String result = executorService.invokeAny(callableTasks);

invokeAll() assigns a collection of tasks to an ExecutorService, causing each to run, and returns the result of all task executions in the form of a list of objects of type Future:

	List<Future<String>> futures = executorService.invokeAll(callableTasks);


## Fork/Join Framework

https://www.baeldung.com/java-fork-join

The fork/join framework was presented in Java 7. It provides tools to help speed up parallel processing by attempting to use all available processor cores – which is accomplished through a divide and conquer approach.

In practice, this means that the framework first “forks”, recursively breaking the task into smaller independent subtasks until they are simple enough to be executed asynchronously.

After that, the “join” part begins, in which results of all subtasks are recursively joined into a single result, or in the case of a task which returns void, the program simply waits until every subtask is executed.

To provide effective parallel execution, the fork/join framework uses a pool of threads called the ForkJoinPool, which manages worker threads of type ForkJoinWorkerThread.

### RecursiveAction

In the example below, the unit of work to be processed is represented by a String called workload. For demonstration purposes, the task is a nonsensical one: it simply uppercases its input and logs it.

To demonstrate the forking behavior of the framework, the example splits the task if workload.length() is larger than a specified threshold using the createSubtask() method.

The String is recursively divided into substrings, creating CustomRecursiveTask instances which are based on these substrings.

As a result, the method returns a List<CustomRecursiveAction>.

The list is submitted to the ForkJoinPool using the invokeAll() method:

	public class CustomRecursiveAction extends RecursiveAction {
		
		private String workload = "";
		private static final int THRESHOLD = 4;
		
		private static Logger logger = 
		  Logger.getAnonymousLogger();
		  
		public CustomRecursiveAction(String workload) {
			this.workload = workload;
		}
		
		@Override
		protected void compute() {
			if (workload.length() > THRESHOLD) {
				ForkJoinTask.invokeAll(createSubtasks());
			} else {
			   processing(workload);
			}
		}
		
		private List<CustomRecursiveAction> createSubtasks() {
			List<CustomRecursiveAction> subtasks = new ArrayList<>();
			
			String partOne = workload.substring(0, workload.length() / 2);
			String partTwo = workload.substring(workload.length() / 2, workload.length());
			
			subtasks.add(new CustomRecursiveAction(partOne));
			subtasks.add(new CustomRecursiveAction(partTwo));
			
			return subtasks;
		}
		
		private void processing(String work) {
			String result = work.toUpperCase();
			logger.info("This result - (" + result + ") - was processed by " 
				+ Thread.currentThread().getName());
		}
	}

This pattern can be used to develop your own RecursiveAction classes. To do this, create an object which represents the total amount of work, chose a suitable threshold, define a method to divide the work, and define a method to do the work.

### RecursiveTask<V>

For tasks that return a value, the logic here is similar, except that the result for each subtask is united in a single result:

	public class CustomRecursiveTask extends RecursiveTask<Integer> {
		private int[] arr;
		
		private static final int THRESHOLD = 20;
		
		public CustomRecursiveTask(int[] arr) {
			this.arr = arr;
		}
		
		@Override
		protected Integer compute() {
			if (arr.length > THRESHOLD) {
				return ForkJoinTask.invokeAll(createSubtasks())
					.stream()
					.mapToInt(ForkJoinTask::join)
					.sum();
			} else {
				return processing(arr);
			}
		}
		
		private Collection<CustomRecursiveTask> createSubtasks() {
			List<CustomRecursiveTask> dividedTasks = new ArrayList<>();
			dividedTasks.add(new CustomRecursiveTask(
				Arrays.copyOfRange(arr, 0, arr.length / 2)));
			dividedTasks.add(new CustomRecursiveTask(
				Arrays.copyOfRange(arr, arr.length / 2, arr.length)));
			return dividedTasks;
		}
		
		private Integer processing(int[] arr) {
			return Arrays.stream(arr)
				.filter(a -> a > 10 && a < 27)
				.map(a -> a * 10)
				.sum();
		}
	}

In this example, the work is represented by an array stored in the arr field of the CustomRecursiveTask class. The createSubtasks() method recursively divides the task into smaller pieces of work until each piece is smaller than the threshold. Then, the invokeAll() method submits the subtasks to the common pool and returns a list of Future.

To trigger execution, the join() method is called for each subtask.

In this example, this is accomplished using Java 8's Stream API; the sum() method is used as a representation of combining sub results into the final result.


## CompletableFuture

`CompletableFuture` is an implementation of Future that places callbacks, executed in a different thread from the task, on the same footing as synchronous continuation functions, executed in the same thread. It avoids the biggest problem of conventional callbacks, that of separating the flow of control into different event handlers, by allowing `CompletableFuture` instances to be composed with callback methods to form new `CompletableFuture`s.

The code below is imagined to be part of a web page renderer whose task for each page is to render its text and also to download and render its images. Image downloads take a long time, during which the CPU has nothing to do but wait. So the obvious strategy renders a page by first initiating the downloads for all its images, then using the time before they complete to render the page text, and finally rendering the downloaded images.

Some simplifying assumptions make this example and its subsequent variants manageable: we assume the existence of types `ImageInfo` (roughly, a URL) and `ImageData` (the binary data of an image), and of methods `scanForImageInfo`, `downloadImage`, `renderText`, `renderImage`, `launderThrowable`, and `ImageInfo.downloadImage`. The instance variable `executor` would have been declared with type `ExecutorService` and appropriately initialized. 

### Image Download with `Future`

	public void renderPage(CharSequence source) {
	List<ImageInfo> info = scanForImageInfo(source);
	
	// create Callable representing download of all images
	final Callable<List<ImageData>> task = () ->
		info.stream()
			.map(ImageInfo::downloadImage)
			.collect(Collectors.toList());
			
	// submit download task to the executor
	Future<List<ImageData>> images = executor.submit(task);
	// renderText(source);
	try {
	   // get all downloaded images (blocking until all are available)
	   final List<ImageData> imageDatas = images.get();
	   // render images
	   imageDatas.forEach(this::renderImage);
	} catch (InterruptedException e) {
	   // Re-assert the thread’s interrupted status
	   Thread.currentThread().interrupt();
	   // We don’t need the result, so cancel the task too
	   images.cancel(true);
	} catch (ExecutionException e) {
	  throw launderThrowable(e.getCause());
	}
  
The obvious drawback is the coarse-grained nature of the download task; no images are available for rendering until all have been downloaded.

### Image Download with `CompletableFuture`

	public void renderPage(CharSequence source) { 
		List<ImageInfo> info = scanForImageInfo(source); 
		info.forEach(imageInfo -> 
			CompletableFuture 
				.supplyAsync(imageInfo::downloadImage) 
				.thenAccept(this::renderImage)); 
		renderText(source); 
	}

This downloads the images via `supplyAsync` which executes the `imageInfo::downloadImage` method in the common `ForkJoinPool`.  It returns a `CompletableFuture` on which one of the `then...` functions can be called, in this case `thenAccept` which "accepts" the result, passing it to the `renderImage` method.  The results are supplied as they become available, allowing images to be rendered as their download finishes.

To be sure, thenAccept is not the only way of composing a `CompletableFuture` with a function. Methods that compose `CompletableFuture`s with functions can take the following arguments:

- a function that will be applied to the `CompletableFuture`’s result. The methods that accept these are:
	- `thenCompose`, for functions that return a CompletableFuture
	- `thenApply`, for functions that return other types;
	- `thenAccept` for functions that return void;
- a `Runnable`. This is accepted by the method thenRun;
- functions that can handle both normal and exceptional termination. `CompletableFuture` composes both respectively with the methods:
	- `handle`, for functions that take a value and a Throwable and return a value;
	- `whenComplete`, for functions that take a value and a Throwable and return void.
