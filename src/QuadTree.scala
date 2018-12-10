import scala.collection.mutable.ArrayBuffer

case class Point(var x: Double, var y: Double)

case class Box(lowerBound: Point, upperBound: Point){
  def center = Point((upperBound.x + lowerBound.x) / 2, (upperBound.y + lowerBound.y) / 2)
  def contains(p: Point): Boolean =
    p.x > lowerBound.x && p.x <= upperBound.x && p.y > lowerBound.y && p.y <= upperBound.y
}

class QuadTree[A](K: Int = 2) {
  private var root = new Node(K, Box(Point(-1000, -1000), Point(1000, 1000)))

  def build(points: Iterable[Point]): Unit = root.build(points)
  def insert(p: Point): Unit = {root = root.insert(p)}
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
    // For iterative access
    def children = Array(topLeft, topRight, bottomLeft, bottomRight)

    var elements: ArrayBuffer[Point] = new ArrayBuffer[Point]()

    def isLeaf: Boolean = topLeft == null
    def center: Point = bounds.center

    // Bound calculators
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

    def insert(point: Point): Node = {
      def split(): Unit = {
        topLeft = new Node(K, topLeftBounds)
        bottomLeft = new Node(K, bottomLeftBounds)
        topRight = new Node(K, topRightBounds)
        bottomRight = new Node(K, bottomRightBounds)
        elements.foreach(p => findSubtree(p).insert(p))
        elements.clear()
      }

      def expand(): Node = {
        throw new NotImplementedError("Expanding not implemented yet.")
        this
      }

      def insertElement(): Unit = {
        if (!elements.contains(point)) elements += point
        if (elements.size > K) split()
      }

      // Check if bounds, else recursively expand
      if (!bounds.contains(point)) {
        return expand().insert(point)
      }

      if (isLeaf)
        insertElement()
      else
        findSubtree(point).insert(point)

      this
    }

    def remove(point: Point): Boolean = {
      def mustCollapse = !isLeaf && children.forall(_.isLeaf) && children.foldLeft(0) {_ + _.elements.size} <= K

      def collapse(): Unit ={
        elements = children.foldLeft(ArrayBuffer[Point]()) { _ ++ _.elements}
        topLeft = null
        topRight = null
        bottomLeft = null
        bottomRight = null
      }

      def removeElement(): Boolean = {
        val containsPoint = elements.contains(point)
        if (containsPoint)
          elements -= point

        containsPoint
      }

      if(!bounds.contains(point))
        return false

      if(isLeaf) {
        removeElement()
      }
      else {
        val foundPoint = findSubtree(point).remove(point)
        if(foundPoint && mustCollapse)
          collapse()
        foundPoint
      }
    }

    def update(point: Point): Boolean = {
      def updateElement(): Boolean = {
        val i = elements.indexOf(point)

        if(i == -1) {
          false
        } else {
          println("No data for update yet")
          true //elements(i) =
        }
      }

      if (!bounds.contains(point)) {
        false
      } else if (isLeaf) {
        updateElement()
      } else {
        findSubtree(point).update(point)
      }
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
      // Potential no such element exception
      children.find(_.bounds.contains(p)).get
    }
  }
}