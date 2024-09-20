package example

import Recursion._

class RecursionSpec extends munit.FunSuite {
  test("factorial") {
    assertEquals(factorial(4), 24)
    assertEquals(factorial(2), 2)
    assertEquals(factorial(1), 1)
    assertEquals(factorial(0), 1)
  }
}
