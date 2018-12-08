import scala.collection.mutable.ListBuffer

object Main extends App {
  var tree = new QuadTree[String](2)
  var points = ListBuffer(Point(1,2), Point(1,4), Point(4,1))
  tree.build(points)
  tree.insert(Point(4,4))
  points += Point(4,4)
  points += Point(-1, -1) // Not in tree

  for(p <- points){
    if (tree.search(p))
      println("Found " + p)
    else
      println("NOT Found " + p)
  }
}
