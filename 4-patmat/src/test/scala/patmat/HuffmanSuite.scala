package patmat

class HuffmanSuite extends munit.FunSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(weight(t1), 5)
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(chars(t2), List('a','b','d'))
    }
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times helper") {
    assertEquals(times("aaabccbba".toCharArray.toList), List(('a', 4), ('b', 3), ('c', 2)))
  }

  test("ordered leaf list helper custom") {
    assertEquals(makeOrderedLeafList(times("aaabccbba".toCharArray.toList)), List(Leaf('c', 2), Leaf('b', 3), Leaf('a', 4)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("french decoder") {
    assertEquals(decodedSecret.mkString, "huffmanestcool")
  }

  test("french encode slow") {
    assertEquals(encode(frenchCode)("huffmanestcool".toCharArray.toList), secret)
  }

  test("french encode quick") {
    assertEquals(quickEncode(frenchCode)("huffmanestcool".toCharArray.toList), secret)
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
