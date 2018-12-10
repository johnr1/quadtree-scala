import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

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

}
