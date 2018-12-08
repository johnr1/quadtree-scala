import scala.collection.mutable.ListBuffer

case class Point(x: Double, y: Double)

case class Box(lowerBound: Point, upperBound: Point){
  def center = Point((upperBound.x + lowerBound.x) / 2, (upperBound.y + lowerBound.y) / 2)
  def contains(p: Point): Boolean =
    p.x > lowerBound.x && p.x < upperBound.x && p.y > lowerBound.y && p.y < upperBound.y
}

class QuadTree[A](K: Int = 2) {
  private var root = new Node(K, Box(Point(-5, -5), Point(5, 5)))

  def build(points: Iterable[Point]): Unit = root.build(points)
  def insert(p: Point): Unit = root.insert(p)
  def remove(p: Point): Unit = root.remove(p)
  def search(p: Point): Boolean = root.search(p)
  def update(p: Point): Unit = root.update(p)
  def rangeSearch(p1: Point, p2: Point): Unit = root.rangeSearch(p1, p2)
  def kNNSearch(p: Point): Unit = root.kNNSearch(p)

  class Node(K: Int, var bounds: Box = null) {
    var topLeft: Node = _
    var topRight: Node = _
    var bottomLeft: Node = _
    var bottomRight: Node = _
    var elements: ListBuffer[Point] = new ListBuffer[Point]()

    def isLeaf: Boolean = topLeft == null
    def center: Point = bounds.center
    def topRightBounds = Box(center, bounds.upperBound)
    def bottomLeftBounds = Box(bounds.lowerBound, center)
    def topLeftBounds = Box(
      Point(bounds.lowerBound.x, center.y),
      Point(center.x, bounds.upperBound.y))
    def bottomRightBounds = Box(
      Point(center.x, bounds.lowerBound.y),
      Point(bounds.upperBound.x, center.y))

    /* Public Functions */

    def build(points: Iterable[Point]): Unit = points.foreach(insert)

    def insert(point: Point): Unit = {
      def split(): Unit = {
        topLeft = new Node(K, topLeftBounds)
        bottomLeft = new Node(K, bottomLeftBounds)
        topRight = new Node(K, topRightBounds)
        bottomRight = new Node(K, bottomRightBounds)
        elements.foreach(p => findSubtree(p).insert(p))
        elements.clear()
      }

      def expand(): Node = {
        throw new NotImplementedError("Expanding not implemented yet")
      }

      // Check if bounds, else recursively expand
      if (!bounds.contains(point)) {
        expand().insert(point)
        return
      }

      if (isLeaf){
        elements += point
        if(elements.size > K) split()
      }else {
        findSubtree(point).insert(point)
      }
    }

    def remove(point: Point): Unit = {
      throw new NotImplementedError("delete not implemented yet")
    }

    def update(point: Point): Unit = {
      throw new NotImplementedError("update not implemented yet")
    }

    def search(point: Point): Boolean = {
      if (!bounds.contains(point)) {
        false
      } else if (isLeaf) {
        elements.contains(point)
      } else {
        findSubtree(point).search(point)
      }
    }

    def kNNSearch(point: Point): Unit = {
      throw new NotImplementedError("kNNSearch not implemented yet")
    }

    def rangeSearch(p1: Point, p2: Point): Unit = {
      throw new NotImplementedError("Range not implemented yet")
    }

    private def findSubtree(p: Point): Node = {
      // Could be unsafe
      Array(bottomLeft, bottomRight, topLeft, topRight)
        .find(_.bounds.contains(p)).get
    }
  }
}