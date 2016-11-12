package week4.List

abstract class List[T] {
  def this() = Empty
  def this(val first: T) = new Node(first, Empty)
  def this(val first: T, val second: T) = new Node(second, new Node(first, Empty))

  def isEmpty: Boolean
  def tail: T
  def long_head: List[T]
}

object Empty[T] extends List[T] {
  def isEmpty = True
  def tail = throw new Error("Empty.tail")
  def long_head = throw new Error("Empty.long_head")
}

def Node[T](val item: T, val pred: List[T]) extends List[T] {
  def isEmpty = False
  def tail = item
  def long_head = pred
}
