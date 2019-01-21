import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.{pow, max, abs}

// FUNDAMENTAL DATA CLASSES
// Comparison <op>, returns true if both x and y are true for <op>
case class Point(var x: Double, var y: Double) {
  def < (other: Point): Boolean = x < other.x && y < other.y
  def > (other: Point): Boolean = x > other.x && y > other.y
  def <= (other: Point): Boolean = x <= other.x && y <= other.y
  def >= (other: Point): Boolean = x >= other.x && y >= other.y
  def mid(other: Point): Point = Point((x + other.x) / 2, (y + other.y) / 2)
  def distance(other: Point): Double = pow(x - other.x, 2) + pow(y - other.y, 2)
  def distance(box: Box): Double = {
    val dx = max(abs(x - box.center.x) - box.halfDim , 0)
    val dy = max(abs(y - box.center.y) - box.halfDim , 0)
    pow(dx, 2) + pow(dy, 2)
  }
}

case class Box(var center: Point, var halfDim: Double){
  var topLeftPoint: Point = Point(center.x + halfDim, center.y - halfDim)
  var topRightPoint: Point = Point(center.x + halfDim, center.y + halfDim)
  var bottomLeftPoint: Point = Point(center.x - halfDim, center.y - halfDim)
  var bottomRightPoint: Point = Point(center.x - halfDim, center.y + halfDim)

  def topLeftSubBox: Box = Box(center.mid(topLeftPoint), halfDim/2)
  def topRightSubBox: Box = Box(center.mid(topRightPoint), halfDim/2)
  def bottomLeftSubBox: Box = Box(center.mid(bottomLeftPoint), halfDim/2)
  def bottomRightSubBox: Box = Box(center.mid(bottomRightPoint), halfDim/2)

  def contains(p: Point): Boolean = p > bottomLeftPoint && p <= topRightPoint
  def overlaps(r: Box): Boolean = bottomLeftPoint <= r.topRightPoint && topRightPoint >= r.bottomLeftPoint
  def overlaps(lowerBound: Point, upperBound: Point): Boolean = bottomLeftPoint <= upperBound && topRightPoint >= lowerBound
}


/**
  * The Class representing a complete QuadTree, most operations are implemented
  * inside the inner Node class and called recursively, The outter methods just call
  * the Node implementations with root as the node.
  *
  * @param K Maximum number of elements that can fit inside a leaf node
  * @tparam A The type of the data saved inside the Tree
  */
class QuadTree[A](K: Int = 2) {
  case class Element(position: Point, data: A)
  private var root = new Node(K, Box(Point(0, 0), 500))

  // Public interface callers, Work is done mostly inside Node Class
  def build(elements: Iterable[(Point, A)]): Unit = elements.foreach( e => insert(e._1, e._2) )
  def insert(p: Point, data: A): Boolean = root.insert(Element(p, data))
  def remove(p: Point): Boolean = root.remove(p)
  def search(p: Point): Option[A] = root.search(p)
  def update(p: Point, data: A): Boolean = root.update(Element(p, data))
  def rangeSearch(fromPos: Point, toPos: Point): ListBuffer[(Point, A)] = root.rangeSearch(fromPos, toPos).map(Element.unapply(_).get)

  def knnSearch(p: Point, k: Integer): ListBuffer[(Point, A)] = {
    if (k <= 0) return new ListBuffer[(Point, A)]()

    object distanceOrdering extends Ordering[Element] {
      def compare(a: Element, b: Element): Int = a.position.distance(p) compare b.position.distance(p)
    }

    val knnElements = new mutable.PriorityQueue[Element]()(distanceOrdering)
    root.knnSearch(p, k, knnElements) // populates knnElements
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


    /* ===== Operation Functions ===== */

    /**
      * Inserts the element into the subtree of the node called
      *
      * Recursively performs insertion to the QuadTree by
      * following the correct children nodes. Once a leaf is reached
      * the Element is inserted. If the number of elements of a leaf node
      * is more than K, then a split operation is performed, creating 4 more
      * child nodes and moving the leaf's elements to them.
      *
      * @param elem The Element object to be inserted
      * @return true if insertion took place or false if not
      */
    def insert(elem: Element): Boolean = {
      def split(): Unit = {
        topLeft = new Node(K, bounds.topLeftSubBox)
        bottomLeft = new Node(K, bounds.bottomLeftSubBox)
        topRight = new Node(K, bounds.topRightSubBox)
        bottomRight = new Node(K, bounds.bottomRightSubBox)

        elements.foreach(e => findSubtree(e.position).insert(e))
        elements.clear()
      }

      def insertElement(): Boolean = {
        var returnValue = false  //Returns true if value actually inserted
        if (!elements.exists(_.position == elem.position)) {
          elements += elem
          returnValue = true
        }
        if (elements.size > K)
          split()

        returnValue
      }

      if (!bounds.contains(elem.position)) {
        throw new IllegalArgumentException(s"The position '${elem.position}' is out of QuadTree bounds.")
      }

      if (isLeaf)
        insertElement()
      else
        findSubtree(elem.position).insert(elem)
    }


    /**
      * Removes an element from the subtree of the node called
      *
      * Recursively performs remove to the correct child of
      * the QuadTree nodes until it reaches a leaf node. If the leaf
      * node contains the element with position point, the element is removed
      *
      * @param point The position of the element to be removed
      * @return true if the element is removed, false otherwise
      */
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

    /**
      * Updates the data of an element already existing in the tree
      *
      * Recursively calls update to the correct child of the tree nodes
      * until it reaches a leaf. If the leaf node, contains the an element
      * with position, point, then it updates it's data.
      *
      * @param elem The element to be updates
      * @return true if update took place, false otherwise
      */
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

    /**
      * Performs a range search on the Quadtree
      *
      * Recursively calls rangeSearch to all the nodes which overlap
      * with the boundary formed by the range bounds specified.
      * When a leaf is reached, it returns a list of the elements inside the
      * range. While the recursion unravels, the lists as cconcatenated returning
      * a full list of all the elements in the range inside the tree.
      *
      * @param fromPos The {xFrom, yFrom} point of the range search
      * @param toPos The {xTo, yTo} point of the range search
      * @return A list of elements inside the specified range.
      */
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


    def knnSearch(point: Point, k: Integer, elementsSoFar: mutable.PriorityQueue[Element]): Unit = {
      def explorePotentialNode(n: Node): Unit = {
        def shouldExplore(n: Node): Boolean = elementsSoFar.length < k || point.distance(n.bounds) < elementsSoFar.head.position.distance(point)

        if (shouldExplore(n))
          n.knnSearch(point, k, elementsSoFar)
      }

      def addPotentialElement(e: Element): Unit = {
        def isPotentialNN(e: Element): Boolean = elementsSoFar.length < k || e.position.distance(point) < elementsSoFar.head.position.distance(point)

        if (isPotentialNN(e)) {
          elementsSoFar.enqueue(e)
          if (elementsSoFar.length > k) elementsSoFar.dequeue()
        }
      }


      if(isLeaf){
        elements.foreach(addPotentialElement)
      }
      else if(bounds.contains(point)){
        val diveNode = findSubtree(point)
        diveNode.knnSearch(point, k, elementsSoFar)
        children.filterNot(_ == diveNode).foreach(explorePotentialNode)
      }
      else {
        children.foreach(explorePotentialNode)
      }
    }


    /**
      * Return the child node which bounds, contain the position point
      *
      * @param p
      * @return The child node
      */
    private def findSubtree(p: Point): Node = {
      // Potential no such element exception
      children.find(_.bounds.contains(p)).get
    }
  }
}