# QuadTree

A complete QuadTree data structure implementation in Scala. 

Supports the following operations:
* Insertion
* Search
* Deletion
* Update
* kNN Search
* Range Search
* Quadtree nodes and 2D plane visualizations

## Usage
The QuadTree can associate a Point with coordinates x,y with any datatype using Scala Generics. 

```scala
// Creates a new Quadtree that associates points with Strings. Its center is (0,0)
// and bounds x:(-50,50) y:(-50,50) where each leaf can store up two elements
scala> val q = new QuadTree[String](K=2, center=Point(0,0), halfDim=50)
q: QuadTree[String] = QuadTree@21710383

// Insert a point (1,5) with value "String1"
scala> q.insert(Point(1,5), "String1")
res1: Boolean = true  // Returns true to indicate successful insertion (would return false if already existed)

scala> q.insert(Point(5,5), "String2")
res2: Boolean = true

scala> q.insert(Point(-9, 0), "String3")
res3: Boolean = true

// Search for point 5,5 in the tree
scala> q.search(Point(5, 5))
res4: Option[String] = Some(String2)

// Remove point 5,5
scala> q.remove(Point(5, 5))
res5: Boolean = true // True indicates success

scala> q.search(Point(5,5))
res6: Option[String] = None // Item deleted

// Search for 1 nearest neighbor to the point 0,0
scala> q.knnSearch(Point(0,0), 1)
res7: scala.collection.mutable.ListBuffer[(Point, String)] = ListBuffer((Point(1.0,5.0),String1))

// Search for all points in the range x: -100 to 0 and y: -100 to 0:
scala> q.rangeSearch(Point(-100,-100), Point(0,0))
res8: scala.collection.mutable.ListBuffer[(Point, String)] = ListBuffer((Point(-9.0,0.0),String3))
```

### Visualizations
`q.toGraphvizString()` generates the graphviz dot to file format to visualize the tree.

`q.toGnuPlotString()` to generate the gnu plot format to visualize the 2D plane.

## Building

In order to compile the library and run the demos you will need theese tools installed:
* scala
* sbt

Open a command line and type 

```bash
sbt compile
```
To compile the .scala files and generate the .class Files


If you would like to generate a .jar package type:
```bash
sbt package
```

## Running showcasing 'Main' class
The library includes a Main class which can perform certain tasks depending on the command line arguments.
These tasks are included in the help which is printed when running the program without any arguments.
The tasks are:
* ``time``: Times the time to run all quadtree operations on random data.
* ``demo``: Performs a demo of all operations (except range search), with a small number of points (20 by default), printing the results and generating the graphviz and gnu plot files of the graph created.
* ``test``: Performs a series of tests on all operations with random data, testing the integrity of the results of the quadtree.

To run the Main class type :
```bash
sbt run
```

And you should see 
```
Usage: quadtree command 

Commands: 
  time [-n elements]:   Performs all basic operations (except range search) of n elements, and prints the time to complete 
  test [-i iterations]: Performs and validates all quadtree operations with random data 
  demo [-n elements]:   Performs a demo of all QuadTree operations with random data. Prints the tree generated in .dot format 
```

## Examples

### Performing a demo run with 100 random elements:
```
sbt "run demo -n 100"
```
> Notice the quotes following the sbt command (needed to pass parameters to the Main executable)


### Performing an integrity test of all operations:
```
sbt "run test"
```

### Timing QuadTree operations with 100.000 elements:
```
sbt "run time -n 100000"
```

### Entering interactive scala REPL
```
sbt console
```
From here you can use the entire Quadtree API from the terminal e.g.

```scala
[info] Starting scala interpreter...
Welcome to Scala 2.12.8 (OpenJDK 64-Bit Server VM, Java 1.8.0_202).
Type in expressions for evaluation. Or try :help.

scala> val q = new QuadTree[String](halfDim=50); // Create a quadtree holding strings from -50,-50 to 50,50
q: QuadTree[String] = QuadTree@73b5f09b

scala> q.insert(Point(5,5), "Hello"); // Insert value "Hello" at 5,5
res0: Boolean = true

scala> q.insert(Point(6,6), "World"); // Insert value "World" at 6,6
res1: Boolean = true

scala> q.search(Point(6,6)); // Insert value "World" at 6,6
res2: Option[String] = Some(World)
```
