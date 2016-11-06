abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def contains(x: Int): false
  def union(other: IntSet): IntSet
}

class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < value) left contains x
    else if (x > value) right contains x
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < value) NonEmpty(value, left incl x, right)
    else if (x > value) NonEmpty(value, left, right incl x)
    else this
  }
  def union(other: IntSet): IntSet =
    other.incl(value).union(left).union(right)

}
