import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Tests {
  val r: Random.type = scala.util.Random

  def testBuild(): Boolean = {
    val ELEMENTS_TO_INSERT = 50000
    val tree = new QuadTree[Int]()
    var elements = mutable.HashMap[Point, Int]()

    for (_ <- 0 to ELEMENTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      elements += p -> d
    }

    tree.build(elements.toList)

    for (e <- elements) {
      val res = tree.search(e._1)
      if (res.isEmpty || tree.search(e._1).get != e._2) {
        println("Error, wrong data returned for point: " + e._1)
        return false
      }
    }

    true
  }

  def testInsertions(): Boolean = {
    val ELEMENTS_TO_INSERT = 50000
    val tree = new QuadTree[Int]()
    var elements = mutable.HashMap[Point, Int]()

    for (_ <- 0 to ELEMENTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      elements += p -> d
      tree.insert(p, d)
    }

    for (e <- elements) {
      val res = tree.search(e._1)
      if (res.isEmpty || tree.search(e._1).get != e._2) {
        println("Error, wrong data returned for point: " + e._1)
        return false
      }
    }

    true
  }


  def testRemovals(): Boolean = {
    val ELEMENTS_TO_INSERT = 50000
    val ELEMENTS_TO_REMOVE = (ELEMENTS_TO_INSERT * 0.8).toInt

    val tree = new QuadTree[Int]()
    var points = ArrayBuffer[Point]()
    var elementsInTree = mutable.HashMap[Point, Int]()

    for (_ <- 0 to ELEMENTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      points += p
      elementsInTree += p -> d
      tree.insert(p, d)
    }

    for (i <- 0 to ELEMENTS_TO_REMOVE) {
      val p = points(i)
      tree.remove(p)
      elementsInTree.remove(p)
    }

    for (p <- points) {
      if (elementsInTree.get(p).isDefined && tree.search(p).isEmpty) {
        println("Error, could not find points that should be inside tree: " + p)
        return false
      }

      if (elementsInTree.get(p).isEmpty && tree.search(p).isDefined) {
        println("Error, found points that should have been removed: " + p)
        return false
      }

      if (elementsInTree.get(p).isDefined && tree.search(p).isDefined) {
        if (elementsInTree(p) != tree.search(p).get) {
          println("Error Value Mismatch on removal")
          return false
        }
      }
    }

    true
  }

  def testUpdates(): Boolean = {
    val ELEMENTS_TO_INSERT = 50000
    val ELEMENTS_TO_UPDATE = 40000
    val tree = new QuadTree[Int]()
    var points = ArrayBuffer[Point]()
    var elements = mutable.HashMap[Point, Int]()

    for (_ <- 0 to ELEMENTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      points += p
      elements += p -> d
      tree.insert(p, d)
    }

    for (i <- 0 to ELEMENTS_TO_UPDATE) {
      val p = points(i)
      val d = r.nextInt()
      elements(p) = d
      tree.update(p, d)
    }

    for (e <- elements) {
      val res = tree.search(e._1)
      if (res.isEmpty || tree.search(e._1).get != e._2) {
        println("Error, wrong data returned for point: " + e._1)
        return false
      }
    }

    true
  }

  def testRangeSearch(): Boolean = {
    val POINTS_TO_INSERT = 20000
    val RANGE_SEARCHES_TO_RUN = 100
    val tree = new QuadTree[Int]()
    var points = mutable.HashMap[Point, Int]()

    for (_ <- 0 to POINTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      points += p -> d
      tree.insert(p, d)
    }

    for (_ <- 0 to RANGE_SEARCHES_TO_RUN) {
      val p1 = Point(-450 + 900* r.nextDouble(), -450 + 900* r.nextDouble())
      val p2 = Point(p1.x + (-450 + 900* r.nextDouble()), p1.y + (-450 + 900* r.nextDouble()))

      val filteredPoints = points.filterKeys(p => p >= p1 && p <= p2).toSet
      val treePoints = tree.rangeSearch(p1, p2).toSet

      if (filteredPoints != treePoints) {
        println("Error in RangeSearch, please check data structure (or test :P)")
        return false
      }
    }

    true
  }

  def testKNNSearch(): Boolean = {
    val POINTS_TO_INSERT = 20000
    val KNN_SEARCHES_TO_RUN = 100
    val tree = new QuadTree[Int]()
    var points = mutable.ArrayBuffer[Point]()

    for (_ <- 0 to POINTS_TO_INSERT) {
      val p = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())
      val d = r.nextInt()
      points += p
      tree.insert(p, d)
    }

    for (i <- 1 to KNN_SEARCHES_TO_RUN) {
      val sPoint = Point(-999 + 1998* r.nextDouble(),-999 + 1998* r.nextDouble())

      val treePoints = tree.knnSearch(sPoint, i).map(_._1).toSet
      val sortedPoints = points.sortBy(_.distance(sPoint)).take(i).toSet

      if (treePoints != sortedPoints) {
        println("Error in KNN Search, please check data structure (or test :P)")
        return false
      }
    }

    true
  }

  def runAllTests(iterations: Integer = 10): Unit = {
    def runTest( test: () => Boolean, testTitle: String ): Unit = {
      if (test())
        println("Passed " + testTitle + " Test")
      else
        println("!!! " + testTitle + " Tests failed. Structure not correct !!!")
    }

    val tests = Array(
      (Tests.testBuild _ , "Build"),
      (Tests.testInsertions _ , "Insertion"),
      (Tests.testRemovals _ , "Removal"),
      (Tests.testUpdates _ , "Update"),
      (Tests.testRangeSearch _ , "Range Search"),
      (Tests.testKNNSearch _ , "KNN Search"),
    )

    for(i <- 1 to iterations) {
      println(s" - Iteration: $i - ")
      for (t <- tests) {
        runTest(t._1, t._2)
      }
    }
  }

}
