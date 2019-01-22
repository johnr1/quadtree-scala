object Main extends App {

  /**
    * Prints command line usage
    */
  def printUsage: Unit = {
    println(s"Usage: quadtree command \n\n" +
      s"Commands: \n" +
      s"  time [-n elements]: \tPerforms all basic operations (except range search) of n elements, and prints the time to complete \n" +
      s"  test [-i iterations]: Performs and validates all quadtree operations with random data \n" +
      s"  demo [-n elements]: \tPerforms a demo of all QuadTree operations with random data. Prints the tree generated in .dot format \n")
  }


  if(args.length <= 0) {
    printUsage
    Demo.operationsDemo()
  }
  else if(args(0) == "test") {
    var i = 10
    if(args.length > 2)  i = args(3).toInt
    println(s"Running $i iterations of all operation validations.")
    Tests.runAllTests(i)
  }
  else if(args(0) == "time") {
    var n = 80000
    if(args.length > 2) n = args(3).toInt
    println(s"Performing and timing operations with $n elements.")
    Demo.basicTimeRun(n)
  }
  else if (args(0) == "demo") {
    var n = 20
    if(args.length > 2) n = args(3).toInt
    println(s"Performing a demo of all operations with $n elements.")
    Demo.operationsDemo(n)
  }
  else {
    printUsage
  }

}
