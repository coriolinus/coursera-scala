package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s = union(s1, s2)  // {1, 2}
    val t = union(s2, s3)  // {2, 3}
    val u = union(s, t)    // {1, 2, 3}
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("singletonSet(1) doesn't contain 0") {
    new TestSets {
      assert(! contains(s1, 0), "not Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements in both sets"){
    new TestSets {
      val i = intersect(s, t)
      assert(contains(i, 2), "intersect 1")
      assert(!contains(i, 3), "intersect 3")
      assert(!contains(i, 1), "intersect 2")
    }
  }

  test("diff contains elements from s but not t"){
    new TestSets {
      val v = diff(u, s2)    // {1, 3}
      assert(contains(v, 1))
      assert(!contains(v, 2))
      assert(contains(v, 3))
      val w = diff(u, s)     // {3}
      assert(!contains(w, 1))
      assert(!contains(w, 2))
      assert(contains(w, 3))
    }
  }

  test("filter contains elements for which predicate is true") {
    new TestSets {
      val g1 = filter(u, (p: Int) => p > 1)
      assert(!contains(g1, 1))
      assert(contains(g1, 2))
      assert(contains(g1, 3))

      val le2 = filter(u, (p: Int) => p <= 2)
      assert(contains(le2, 1))
      assert(contains(le2, 2))
      assert(!contains(le2, 3))
    }
  }

  test("forall returns true if p is true for all members of s") {
    new TestSets {
      assert(forall(u, (p: Int) => p > 0))
      assert(!forall(u, (p: Int) => p > 1))
      assert(!forall(u, (p: Int) => p < 3))
      assert(forall(union(s1, s3), (p: Int) => p != 2))
    }
  }

  test("exists returns true if any member of s returns true for p") {
    new TestSets {
      assert(exists(u, (p: Int) => p % 2 == 0))
      assert(!exists(u, (p: Int) => p < 0))
    }
  }

  test("map correctly transforms elements of sets") {
    new TestSets {
      // u = {1, 2, 3}
      val w = map(u, (i: Int) => i * 2)  // w is doubled u = {2, 4, 6}
      assert(forall(w, (i: Int) => i % 2 == 0))
      assert(!exists(w, (i: Int) => i % 2 != 0))
      assert(forall(w, (i: Int) => i >= 2 && i <= 6))
      assert(!exists(w, (i: Int) => i < 2 || i > 6))

      val n = map(u, (i: Int) => i * -1) // n is negated u = {-1, -2, -3}
      assert(forall(n, (i: Int) => i < 0))
      assert(!exists(n, (i: Int) => i >= 0))
      assert(forall(n, (i: Int) => i >= -3 && i <= -1))
      assert(!exists(n, (i: Int) => i < -3 || i > -1))
    }
  }
}
