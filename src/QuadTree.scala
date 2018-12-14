import scala.collection.mutable.ListBuffer

// Comparison <op>, returns true if both x and y are true for <op>
case class Point(var x: Double, var y: Double) {
  def <(p: Point): Boolean = x < p.x && y < p.y
  def >(p: Point): Boolean = x > p.x && y > p.y
  def <= (p: Point): Boolean = x <= p.x && y <= p.y
  def >= (p: Point): Boolean = x >= p.x && y >= p.y
}

case class Box(lowerBound: Point, upperBound: Point){
  def center = Point((upperBound.x + lowerBound.x) / 2, (upperBound.y + lowerBound.y) / 2)
  def contains(p: Point): Boolean = p > lowerBound && p <= upperBound
  def overlaps(b: Box): Boolean = lowerBound <= b.upperBound && upperBound >= b.lowerBound
}

class QuadTree[A](K: Int = 2) {
  case class Element(position: Point, data: A)
  private var root = new Node(K, Box(Point(-1000, -1000), Point(1000, 1000)))

  def build(elements: Iterable[Element]): Unit = {elements.foreach( e => root = root.insert(e) )}
  def insert(position: Point, data: A): Unit = {root = root.insert(Element(position, data))}
  def remove(position: Point): Boolean = root.remove(position)
  def search(position: Point): Option[A] = root.search(position)
  def update(position: Point, data: A): Boolean = root.update(Element(position, data))
  def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[(Point, A)] = root.rangeSearch(fromPos, toPos)
  def kNNSearch(position: Point): Unit = root.kNNSearch(position)

  class Node(K: Int, var bounds: Box = null) {
    var topLeft: Node = _
    var topRight: Node = _
    var bottomLeft: Node = _
    var bottomRight: Node = _
    def children = Array(topLeft, topRight, bottomLeft, bottomRight) // For iterative access
    var elements: ListBuffer[Element] = new ListBuffer[Element]()

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

    /* Operation Functions */
    def insert(elem: Element): Node = {
      def split(): Unit = {
        topLeft = new Node(K, topLeftBounds)
        bottomLeft = new Node(K, bottomLeftBounds)
        topRight = new Node(K, topRightBounds)
        bottomRight = new Node(K, bottomRightBounds)
        elements.foreach(e => findSubtree(e.position).insert(e))
        elements.clear()
      }

      def expand(): Node = {
        throw new NotImplementedError("Expanding not implemented yet.")
        this
      }

      def insertElement(): Unit = {
        if (!elements.exists(_.position == elem.position)) elements += elem
        if (elements.size > K) split()
      }

      // Check if bounds, else recursively expand
      if (!bounds.contains(elem.position)) {
        return expand().insert(elem)
      }

      if (isLeaf)
        insertElement()
      else
        findSubtree(elem.position).insert(elem)

      this
    }

    def remove(point: Point): Boolean = {
      def mustCollapse = !isLeaf && children.forall(_.isLeaf) && children.foldLeft(0) {_ + _.elements.size} <= K

      def collapse(): Unit ={
        elements = children.foldLeft(ListBuffer[Element]()) { _ ++ _.elements}
        topLeft = null
        topRight = null
        bottomLeft = null
        bottomRight = null
      }

      def removeElement(): Boolean = {
        val i = elements.indexWhere(_.position == point)
        if (i != -1)
          elements.remove(i)

        i != -1  // Returns true if element removed
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

    def update(elem: Element): Boolean = {
      def updateElement(): Boolean = {
        val i = elements.indexWhere(_.position == elem.position)
        if(i != -1) {
          elements(i) = elem
        }
        i != -1 // Element updated condition
      }

      if (!bounds.contains(elem.position)) {
        false
      } else if (isLeaf) {
        updateElement()
      } else {
        findSubtree(elem.position).update(elem)
      }
    }

    def search(point: Point): Option[A] = {
      if (!bounds.contains(point)) {
        null
      } else if (isLeaf) {
        elements.find(_.position == point).map(_.data)
      } else {
        findSubtree(point).search(point)
      }
    }

    def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[(Point, A)] = {
      if(!(fromPos <= toPos)) return new ListBuffer[(Point, A)]

      if(isLeaf){
        elements.filter(e => e.position >= fromPos && e.position <= toPos)
          .map(e => (e.position, e.data))
      }
      else{
        children.filter(_.bounds.overlaps(Box(fromPos, toPos)))
          .foldLeft(ListBuffer[(Point, A)]()) { _ ++ _.rangeSearch(fromPos, toPos)}
      }
    }

    def kNNSearch(point: Point): Unit = {
      throw new NotImplementedError("kNNSearch not implemented yet")
    }


    private def findSubtree(p: Point): Node = {
      // Potential no such element exception
      children.find(_.bounds.contains(p)).get
    }
  }
}