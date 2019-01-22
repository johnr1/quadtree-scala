import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.util.Random

object Demo {
  /**
    * Generates a dot file from a given tree
    */
  def generateDotFile[A](tree: QuadTree[A], filename: String = "tree.dot"): Unit = {
    val pw = new PrintWriter(new File(filename))
    pw.write(tree.toGraphvizString)
    pw.close()
  }

  /**
    * Performs an insertion, search and deletion of n elements
    * @param n The number of elements to generate
    */
  def basicTimeRun(n: Int): Unit = {
    val r: Random.type = scala.util.Random

    val tree = new QuadTree[Int]()
    val points = new ListBuffer[Point]

    // Generate points
    for (_ <- 0 to n) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      points += p
    }

    def time(treeOperation: Point => Unit, operationName: String): Unit = {
      var t0 = System.nanoTime()
      for (p <- points) {
        treeOperation(p)
      }
      var time = (System.nanoTime() - t0) / 1e6
      println(s"$operationName of $n points: $time ms")
    }

    time(tree.insert(_, 0), "Insertion")
    time(tree.search, "Search")
    time(tree.update(_, 1), "Update")
    time(tree.knnSearch(_, 3), "3NN Search")
    time(tree.remove, "Removal")
  }

  /**
    * Performs a demo of all QuadTree operations with a small
    * amount of random points (to make it easier to keep track of).
    * Prints useful info while executing
    *
    * @param n The number of elements to insert
    */
  def operationsDemo(n: Int = 20): Unit = {
    if(n == 0) return

    val r: Random.type = scala.util.Random
    val tree = new QuadTree[Int](2, Point(50,50), 50)
    var points = new ListBuffer[Point]

    def pointSearch(p: Point): Unit ={
      println(s"[DEMO] Searching for point $p...")
      val e = tree.search(p)
      println(s"[DEMO] Tree result: ${e.getOrElse("None")}")
    }

    // Generate points
    for (_ <- 1 to n) {
      val p = Point(1 + r.nextInt(98), 1 + r.nextInt(98))
      points += p
    }

    points = points.distinct
    println(s"[DEMO] Generated ${points.length} points:")
    points.foreach(println)

    // INSERTION
    println(s"\n[DEMO] Inserting $n points in the tree with data their x+y as Int:")
    points.foreach(p => tree.insert(p, (p.x + p.y).toInt))

    println(s"[DEMO] Generating graphviz format .dot file as 'tree.dot' for tree visualization...\n\n")
    generateDotFile(tree)


    val i = r.nextInt(n)

    // SEARCH
    pointSearch(points(i))

    // UPDATE
    println(s"\n[DEMO] Updating point ${points(i)} value to 8...")
    tree.update(points(i), 8)
    pointSearch(points(i))

    // REMOVAL
    println(s"\n[DEMO] Removing point ${points(i)} from tree...")
    tree.remove(points(i))
    pointSearch(points(i))

    // RANGE SEARCH
    println(s"\n[DEMO] Range searching from x: (22, 44) y: (66, 88)...")
    val rangeResults = tree.rangeSearch(Point(22,66), Point(44, 88))
    println(s"[DEMO] Tree found ${rangeResults.length} points:")
    rangeResults.foreach(println)

    // KNN SEARCH
    val k = 1 + r.nextInt(n/2)
    val p = Point(r.nextInt(99), r.nextInt(99))
    println(s"\n[DEMO] Performing ${k}NN search of point $p")
    val knnResults = tree.knnSearch(p, k)
    println(s"[DEMO] Tree found ${knnResults.length} nearest neighbours:")
    knnResults.foreach(println)

    println("\n[DEMO] COMPLETED")
  }


}
