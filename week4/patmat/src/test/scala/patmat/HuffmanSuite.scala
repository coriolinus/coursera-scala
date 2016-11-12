package patmat

import org.scalatest.FunSuite

import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("count chars in string") {
    val s1 = "bleddaledabblededoo"
    assert(times(s1.toList) ===  List(('b',3), ('l',3), ('e',4), ('d',5), ('a',2), ('o',2)))
  }

  test("Can encode moderately long strings") {
    val s1 = "Here's a moderately long character string. We want to Huffman-encode this thing. This will happen in one of two ways, with pre-computation of results, or without. The assignment claims that pre-computing the results is faster than descending the Huffman tree every time."
    val s2 = "Frankly I'm a little skeptical of that claim. While it's certainly possible that pre-computing the bits of any given character and mapping the characters directly could speed things up, we don't do that, instead searching a list for a character match in order to find its encoding."
    val s3 = "Of course, if we pay attention and ensure that we generate the list in frequency order, that might not be a problem. On the other hand, descending a tree handles that aspect inherently, so I feel like it probably comes out a wash in the end. Overall, I remain skeptical."

    val s_l = (s1 + s2 + s3).toList

    val tree = createCodeTree(s_l)
    val s1_bits = encode(tree)(s1.toList)
    val s2_bits = encode(tree)(s2.toList)
    val s3_bits = encode(tree)(s3.toList)
    val all_bits = encode(tree)(s_l)

    assert(all_bits === s1_bits ::: s2_bits ::: s3_bits)
  }

  test ("Quick encoding produces comparable results to normal encoding") {
    val s1 = "Here's a moderately long character string. We want to Huffman-encode this thing. This will happen in one of two ways, with pre-computation of results, or without. The assignment claims that pre-computing the results is faster than descending the Huffman tree every time."
    val s2 = "Frankly I'm a little skeptical of that claim. While it's certainly possible that pre-computing the bits of any given character and mapping the characters directly could speed things up, we don't do that, instead searching a list for a character match in order to find its encoding."
    val s3 = "Of course, if we pay attention and ensure that we generate the list in frequency order, that might not be a problem. On the other hand, descending a tree handles that aspect inherently, so I feel like it probably comes out a wash in the end. Overall, I remain skeptical."

    val s_l = (s1 + s2 + s3).toList
    val tree = createCodeTree(s_l)
    val all_bits = encode(tree)(s_l)
    val quick_bits = quickEncode(tree)(s_l)

    assert(all_bits === quick_bits)
  }

  test("Encode / decode short texts using quick or not are the same") {
    new TestTrees {
      val s = "abbabab"
      assert(encode(t1)(s.toList) === quickEncode(t1)(s.toList))

      val d = "baddabaddbadab"
      assert(encode(t2)(d.toList) === quickEncode(t2)(d.toList))
    }
  }

}
