package patmat

class HuffmanSuite extends munit.FunSuite:

  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a leaf") {
    assertEquals(weight(Leaf('x', 6)), 6)
  }

  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("char of a leaf") {
    assertEquals(chars(Leaf('b', 3)), List('b'))
  }
  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a', 'b', 'd'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times correctly counts the number of characters") {
    assertEquals(times(string2Chars("aa")), List(('a', 2)))
    assertEquals(times(string2Chars("aabc")), List(('a', 2), ('b', 1), ('c', 1)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton: checks whether the list contains only one single code tree") {
    assert(singleton(List(makeCodeTree(Leaf('x', 1), Leaf('e', 1)))))
    assert(singleton(List(makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    ))))
    assert(!singleton(Nil))
    assert(!singleton(List(makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2)
    ), makeCodeTree(Leaf('x', 1), Leaf('e', 1)))))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until calls the two functions until the list contains only a single tree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val newList = until(singleton, combine)(leaflist)
    assert(singleton(newList))
  }

  test("createCodeTree") {
    val newCodeTree = createCodeTree(string2Chars("bacc"))

    assertEquals(newCodeTree, Fork(
      Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2),
      Leaf('c', 2),
      List('a', 'b', 'c'),
      4))
  }

  test("decode") {
    new TestTrees:
      assertEquals(decode(t1, List(0, 1, 1)), "abb".toList)
      assertEquals(decode(t2, List(1, 0, 0, 0, 1)), "dab".toList)
      println(decodedSecret)
  }
  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }


  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
