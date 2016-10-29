package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = (that: Int) => that == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = (that: Int) => contains(s, that) || contains(t, that)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = (that: Int) => contains(s, that) && contains(t, that)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = (that: Int) => contains(s, that) && !contains(t, that)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  def set_iter(oob: Boolean, cmp: (Boolean, Boolean) => Boolean)(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) oob
      else if (contains(s, a)) cmp(p(a), iter(a + 1))
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean =
      set_iter(true, (a: Boolean, b: Boolean) => a && b)(s, p)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
   def exists(s: Set, p: Int => Boolean): Boolean =
     set_iter(false, (a: Boolean, b: Boolean) => a || b)(s, p)

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
   // *not* s(f(i)); that's the inverse of what we want.
   // that one transforms {1, 2, 3} * 2 into {1}, because only 1 * 2
   // is in the original set.
   // if this were pure math, we could probably get away with s(f^(-1)(i)),
   // but this isn't pure math. We just need to figure out how to
   // get that same effect using only forward transformations.
  def map(s: Set, f: Int => Int): Set = (i: Int) =>
    // i is in s if for some t, f(t) == i and t is in s.
    // can we determine if f(t) is in s for any t without resorting to brute force?
    // possibly not... but we do have a brute-force method handy which will at least
    // solve this problem correctly within the expected bounds:
    exists(s, (t: Int) => f(t) == i && contains(s, t))
    // this computation is ridiculously computationally expensive, and
    // is restricted to the bounds specified, but it does work.
    // We're not doing this for computational efficiency at this point,
    // so much as to prove a point about the capability of pure-functional
    // programming, anyway.


  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
