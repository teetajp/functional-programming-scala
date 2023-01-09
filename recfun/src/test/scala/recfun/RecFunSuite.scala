package recfun

class RecFunSuite extends munit.FunSuite:
  import RecFun.*

  // ------ balance tests -----------------------------------------------------

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }
  test("balance: empty list") {
    assert(balance("".toList))
  }

  // ------ countChange tests -------------------------------------------------

  test("countChange: example given in instructions") {
    assertEquals(countChange(4,List(1,2)), 3)
  }

  test("countChange: sorted CHF") {
    assertEquals(countChange(300,List(5,10,20,50,100,200,500)), 1022)
  }

  test("countChange: no pennies") {
    assertEquals(countChange(301,List(5,10,20,50,100,200,500)), 0)
  }

  test("countChange: unsorted CHF") {
    assertEquals(countChange(300,List(500,5,50,100,20,200,10)), 1022)
  }

  test("countChange: no change needed") {
    assertEquals(countChange(0, List(5,10,20,50,100,200,500)), 1)
  }
  
  test("countChange: no coins to give") {
    assertEquals(countChange(200, List()), 0)
  }

  // ------ pascal tests ------------------------------------------------------

  test("pascal: col=0,row=2") {
    assertEquals(pascal(0, 2), 1)
  }

  test("pascal: col=1,row=2") {
    assertEquals(pascal(1, 2), 2)
  }

  test("pascal: col=1,row=3") {
    assertEquals(pascal(1, 3), 3)
  }
  test("pascal: col=0,row=0") {
    assertEquals(pascal(0, 0), 1)
  }
  test("pascal: col=-1,row=0") {
    assertEquals(pascal(-1, 0), 0)
  }
  test("pascal: col=3,row=2") {
    assertEquals(pascal(3, 2), 0)
  }
  test("pascal: col=2, row=2") {
    assertEquals(pascal(2, 2), 1)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
