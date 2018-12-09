import Main.points

import scala.collection.mutable.ListBuffer

object Main extends App {
  var tree = new QuadTree[String](2)
  var points = ListBuffer(Point(1,2), Point(1,4), Point(4,1))
  tree.build(points)
  tree.insert(Point(4,4))
  points += Point(4,4)
  points += Point(-1, -1) // Not in tree

  searchPoints(points);

  tree.remove(Point(4,4))
  println("== Removed 4,4")
  searchPoints(points)

  println("== Removed all points")
  for(p <- points){
    tree.remove(p)
  }
  searchPoints(points)


  def searchPoints(points: Iterable[Point]): Unit = {
    for(p <- points){
      if (tree.search(p))
        println("== Found " + p)
      else
        println("== NOT Found " + p)
    }
  }

}
