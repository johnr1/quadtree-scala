import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object Tests {
  val r = scala.util.Random

  def testInsertions(): Boolean ={
    val POINTS_TO_INSERT = 50000
    var tree = new QuadTree[Int]()
    var points = ArrayBuffer[Point]()

    for(i <- 0 to POINTS_TO_INSERT) {
      val p = Point(r.nextDouble() % 999, r.nextDouble() % 999)
      points += p
      tree.insert(p)
    }

    for (p <- points){
      if(!tree.search(p)){
        println("Error, could not find inserted point: " + p)
        return false
      }
    }

    points.clear()
    true
  }


  def testRemovals(): Boolean ={
    val POINTS_TO_INSERT = 50000
    val POINTS_TO_REMOVE = (POINTS_TO_INSERT * 0.8).toInt

    var tree = new QuadTree[Int]()
    var points = ArrayBuffer[Point]()
    var isInTree = HashMap[Point, Boolean]()

    for(i <- 0 to POINTS_TO_INSERT) {
      val p = Point(r.nextDouble() % 999, r.nextDouble() % 999)
      points += p
      isInTree += p -> true
      tree.insert(p)
    }

    for(i <- 0 to POINTS_TO_REMOVE){
      tree.remove(points(i))
      isInTree(points(i)) = false
    }

    for (p <- points){
      if(isInTree(p) && !tree.search(p)){
        println("Error, could not find points that should be inside tree: " + p)
        return false
      }

      if(!isInTree(p) && tree.search(p)){
        println("Error, found points that should have been removed: " + p)
        return false
      }
    }

    points.clear()
    true
  }

  def testRangeSearch(): Boolean = {
    val POINTS_TO_INSERT = 20000
    val RANGE_SEARCHES_TO_RUN = 100
    var tree = new QuadTree[Int]()
    var points = HashSet[Point]()

    for(i <- 0 to POINTS_TO_INSERT) {
      val p = Point(r.nextDouble() % 999, r.nextDouble() % 999)
      points += p
      tree.insert(p)
    }

    for (i <- 0 to RANGE_SEARCHES_TO_RUN){
      val p1 = Point(r.nextDouble() % 499, r.nextDouble() % 499)
      val p2 = Point(p1.x + (r.nextDouble() % 499), p1.y + (r.nextDouble() % 499))

      val filteredPoints = points.filter(p => p >= p1 && p <= p2)
      val treePoints = tree.rangeSearch(p1, p2).toSet

      if(filteredPoints != treePoints) {
        println("Error in RangeSearch, please check data structure (or test :P)")
        return false
      }
    }

    points.clear()
    true
  }

}
