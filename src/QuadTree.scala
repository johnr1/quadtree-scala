import scala.collection.mutable.ListBuffer

// FUNDAMENTAL DATA CLASSES
// Comparison <op>, returns true if both x and y are true for <op>
case class Point(var x: Double, var y: Double) {
  def < (p: Point): Boolean = x < p.x && y < p.y
  def > (p: Point): Boolean = x > p.x && y > p.y
  def <= (p: Point): Boolean = x <= p.x && y <= p.y
  def >= (p: Point): Boolean = x >= p.x && y >= p.y
}

case class Box(lowerBound: Point, upperBound: Point){
  def center = Point((upperBound.x + lowerBound.x) / 2, (upperBound.y + lowerBound.y) / 2)
  def contains(p: Point): Boolean = p > lowerBound && p <= upperBound
  def overlaps(b: Box): Boolean = lowerBound <= b.upperBound && upperBound >= b.lowerBound

  def topRightSubBox: Box = Box(center, upperBound)
  def bottomLeftSubBox: Box = Box(lowerBound, center)
  def topLeftSubBox: Box = Box( Point(lowerBound.x, center.y) , Point(center.x, upperBound.y) )
  def bottomRightSubBox: Box = Box( Point(center.x, lowerBound.y) , Point(upperBound.x, center.y) )
}




// QUAD TREE
class QuadTree[A](K: Int = 2) {
  case class Element(position: Point, data: A)
  private var root = new Node(K, Box(Point(-1000, -1000), Point(1000, 1000)))

  def build(elements: Iterable[(Point, A)]): Unit = {elements.foreach( e => root = root.insert(Element(e._1, e._2)) )}
  def insert(p: Point, data: A): Unit = {root = root.insert(Element(p, data))}
  def remove(p: Point): Boolean = root.remove(p)
  def search(p: Point): Option[A] = root.search(p)
  def update(p: Point, data: A): Boolean = root.update(Element(p, data))
  def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[(Point, A)] = root.rangeSearch(fromPos, toPos)
  def kNNSearch(p: Point): ListBuffer[(Point, A)] = root.kNNSearch(p)

  class Node(K: Int, var bounds: Box = null) {
    var topLeft: Node = _
    var topRight: Node = _
    var bottomLeft: Node = _
    var bottomRight: Node = _

    var elements: ListBuffer[Element] = new ListBuffer[Element]()

    def children = Array(topLeft, topRight, bottomLeft, bottomRight) // For iterative access
    def isLeaf: Boolean = topLeft == null


    /* Operation Functions */
    def insert(elem: Element): Node = {
      def split(): Unit = {
        topLeft = new Node(K, bounds.topLeftSubBox)
        bottomLeft = new Node(K, bounds.bottomLeftSubBox)
        topRight = new Node(K, bounds.topRightSubBox)
        bottomRight = new Node(K, bounds.bottomRightSubBox)
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

    def kNNSearch(point: Point): ListBuffer[(Point, A)] = {
      new ListBuffer[(Point, A)]()
      throw new NotImplementedError("kNNSearch not implemented yet")
    }


    private def findSubtree(p: Point): Node = {
      // Potential no such element exception
      children.find(_.bounds.contains(p)).get
    }
  }
}