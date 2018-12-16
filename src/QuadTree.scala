import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.pow

// FUNDAMENTAL DATA CLASSES
// Comparison <op>, returns true if both x and y are true for <op>
case class Point(var x: Double, var y: Double) {
  def < (other: Point): Boolean = x < other.x && y < other.y
  def > (other: Point): Boolean = x > other.x && y > other.y
  def <= (other: Point): Boolean = x <= other.x && y <= other.y
  def >= (other: Point): Boolean = x >= other.x && y >= other.y
  def distance(other: Point): Double = pow(x - other.x, 2) + pow(y - other.y, 2)
  def distance(rect: Box): Double = 0
  def avg(other: Point): Point = Point((x + other.x) / 2, (y + other.y) / 2)
}

case class Box(var center: Point, var halfDim: Double){
  var topLeftPoint: Point = Point(center.x + halfDim, center.y - halfDim)
  var topRightPoint: Point = Point(center.x + halfDim, center.y + halfDim)
  var bottomLeftPoint: Point = Point(center.x - halfDim, center.y - halfDim)
  var bottomRightPoint: Point = Point(center.x - halfDim, center.y + halfDim)

  def topLeftSubBox: Box = Box(center.avg(topLeftPoint), halfDim/2)
  def topRightSubBox: Box = Box(center.avg(topRightPoint), halfDim/2)
  def bottomLeftSubBox: Box = Box(center.avg(bottomLeftPoint), halfDim/2)
  def bottomRightSubBox: Box = Box(center.avg(bottomRightPoint), halfDim/2)

  def contains(p: Point): Boolean = p > bottomLeftPoint && p <= topRightPoint
  def overlaps(r: Box): Boolean = bottomLeftPoint <= r.topRightPoint && topRightPoint >= r.bottomLeftPoint
  def overlaps(lowerBound: Point, upperBound: Point): Boolean = bottomLeftPoint <= upperBound && topRightPoint >= lowerBound
}




// QUAD TREE
class QuadTree[A](K: Int = 2) {
  case class Element(position: Point, data: A)
  private var root = new Node(K, new Box(Point(0, 0), 500))

  def build(elements: Iterable[(Point, A)]): Unit = elements.foreach( e => insert(e._1, e._2) )
  def insert(p: Point, data: A): Unit =   {root = root.insert(Element(p, data))}
  def remove(p: Point): Boolean = root.remove(p)
  def search(p: Point): Option[A] = root.search(p)
  def update(p: Point, data: A): Boolean = root.update(Element(p, data))
  def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[(Point, A)] = root.rangeSearch(fromPos, toPos).map(Element.unapply(_).get)

  def kNNSearch(p: Point, Knn: Integer): ListBuffer[(Point, A)] = {
    object distanceOrdering extends Ordering[Element] {
      def compare(a: Element, b: Element) = a.position.distance(p) compare b.position.distance(p)
    }

    var knnElements = new mutable.PriorityQueue[Element]()(distanceOrdering)
    root.kNNSearch(p, Knn, knnElements) // populates knnElements
    knnElements.to[ListBuffer].map(Element.unapply(_).get)
  }

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

    def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[Element] = {
      if(!(fromPos <= toPos)) return new ListBuffer[Element]

      if(isLeaf){
        elements.filter(e => e.position >= fromPos && e.position <= toPos)
      }
      else{
        children
          .filter(_.bounds.overlaps(fromPos, toPos))
          .foldLeft(ListBuffer[Element]()) { _ ++ _.rangeSearch(fromPos, toPos)}
      }
    }

    def kNNSearch(point: Point, Knn: Integer, itemsSoFar: mutable.PriorityQueue[Element]): ListBuffer[Element] = {
      // dont forget to add ordering to the priority queue
      // Add distance caluclation between box and point
      // Exploring potential node is not ordered. Some have more potential than others

//      def explorePotentialNode(n: Node): Unit = {
//        def shouldExplore(n: Node): Boolean = itemsSoFar.length < Knn || point.distance(n.bounds) < itemsSoFar.head.position.distance(point)
//
//        if (shouldExplore(n))
//          n.kNNSearch(point, Knn, itemsSoFar)
//      }
//
//      def addPotentialElement(e: Element): Unit = {
//        def isPotentialNN(e: Element): Boolean = itemsSoFar.length < Knn || e.position.distance(point) < itemsSoFar.head.position.distance(point)
//
//        if (isPotentialNN(e)) {
//          itemsSoFar.enqueue(e)
//          if (itemsSoFar.length > Knn) itemsSoFar.dequeue()
//        }
//      }
//
//
//      if(isLeaf){
//        elements.foreach(addPotentialElement)
//      }
//      else if(bounds.contains(point)){
//        val diveNode = findSubtree(point)
//        diveNode.kNNSearch(point, Knn, itemsSoFar)
//        children.filterNot(_ == diveNode).foreach(explorePotentialNode)
//      }
//      else {
//        children.foreach(explorePotentialNode)
//      }
//
      new ListBuffer[Element]()
    }



    private def findSubtree(p: Point): Node = {
      // Potential no such element exception
      children.find(_.bounds.contains(p)).get
    }
  }
}