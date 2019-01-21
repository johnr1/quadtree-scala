import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.util.Random

object Main extends App {

  /**
    * Generates a dot file from a given tree
    */
  def generateDotFile[A](tree: QuadTree[A], filename: String = "tree.dot"): Unit = {
    val pw = new PrintWriter(new File(filename))
    pw.write(tree.toGraphvizString)
    pw.close()
  }


  /**
    * Prints command line usage
    */
  def printUsage: Unit = {
    println(s"Usage: quadtree Command \n\n" +
      s"Commands: \n" +
      s"  run [elements]: Performs insertions, searches, and removals of n elements, returns time took to complete \n" +
      s"  test [iterations]: Performs and tests all quadtree operations with random data \n" +
      s"  interactive: loads the greek cities csv file (gr.csv), drops to interactive shell to perform all operations, prints demo information")
  }


  /**
    * Performs an insertion, search and deletion of n elements
    * @param n The number of elements to generate
    */
  def performRun(n: Int = 100000): Unit = {
    val r: Random.type = scala.util.Random

    val tree = new QuadTree[Int]()
    val points = new ListBuffer[Point]

    // Generate points
    for (_ <- 0 to n) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      points += p
    }

    // Time insertion
    var t0 = System.nanoTime()
    for (p <- points) {
      tree.insert(p, 0)
    }
    var time = (System.nanoTime() - t0) / 1e6
    println(s"Insertion of $n points: $time ms")

    // Time Search
    t0 = System.nanoTime()
    for (p <- points) {
      tree.search(p)
    }
    time = (System.nanoTime() - t0) / 1e6
    println(s"Search of $n points: $time ms")

    // Time removal
    t0 = System.nanoTime()
    for (p <- points) {
      tree.remove(p)
    }
    time = (System.nanoTime() - t0) / 1e6
    println(s"Removal of $n points: $time ms")
  }


  // ****** MAIN FUNCTION BODY ******
  if(args.length <= 0){
    val tree = new QuadTree[Int]()
    val r: Random.type = scala.util.Random
    for(_ <- 1 to 30){
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      tree.insert(p, 0)
    }
    tree.bfsPrint
    generateDotFile(tree)

    printUsage
  }
  else if(args(0) == "test"){
    var i = 10
    if(args.length > 1)
      i = args(2).toInt
    Tests.runAllTests(i)
  }
  else if(args(0) == "run"){
    var i = 80000
    if(args.length > 1)
      i = args(2).toInt
      performRun(i)
  } else if (args(0) == "interactive"){
    println("Interactive mode not implemented yet")
  }
  else {
    printUsage
  }

}
