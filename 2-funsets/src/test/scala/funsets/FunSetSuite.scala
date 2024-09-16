package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(_ => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
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
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect finds common elements") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val s4 = union(s1, s2)
      assert(contains(s4, 1))
      assert(contains(s4, 2))
      assert(!contains(s4, 3))
    }
  }

  test("diff does difference correctly") {
    new TestSets {
      val s = union(s1, s2)
      val s4 = diff(s1, s2)
      assert(contains(s4, 1))
      assert(!contains(s4, 2))
      assert(!contains(s4, 3))
    }
  }

  test("filter odds") {
    new TestSets {
      val allSet = union(s1, union(s2, s3))
      assert(contains(allSet, 1))
      assert(contains(allSet, 2))
      assert(contains(allSet, 3))

      val oddSet = filter(allSet, x => x % 2 != 0)
      assert(contains(oddSet, 1))
      assert(!contains(oddSet, 2))
      assert(contains(oddSet, 3))
    }
  }

  test("forall satisfies") {
    new TestSets {
      val allSet = union(s1, union(s2, s3))
      assert(!forall(allSet, x => x % 2 != 0))

      val oddSet = filter(allSet, x => x % 2 != 0)
      assert(forall(oddSet, x => x % 2 != 0))
    }
  }

  test("exists finds") {
    new TestSets {
      assert(!exists(s1, (x) => x % 2 == 0))

      val newSet = union(s1, s2)
      assert(exists(newSet, (x) => x % 2 == 0))
    }
  }

  test("map maps") {
    new TestSets {
      val allSet = union(s1, union(s2, s3))
      val mappedSet = map(allSet, x => x * 2)
      assert(contains(mappedSet, 2))
      assert(contains(mappedSet, 4))
      assert(contains(mappedSet, 6))
    }
  }

  import scala.concurrent.duration._

  override val munitTimeout = 10.seconds
}
