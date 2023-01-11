package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

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
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains all elements that are in both sets") {
    new TestSets:
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersect 1")
  }

  test("intersect does not contain elements that are not in both sets") {
    new TestSets:
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
  }

  test("intersect - composite sets") {
    new TestSets:
      val s = intersect(intersect(s1, s2), s2) // inner intersect set is empty set
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
  }

  test("diff contains elements in first set only and no elements from second set") {
    new TestSets:
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
  }

  test("diff of same sets is empty") {
    new TestSets:
      val s = diff(s1, s1)
      assert(!contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
  }

  test("Filter accepts all elements of the given set that is accepted by predicate p") {
    new TestSets:
      val u_123 = union(union(s1, s2), s3)

      val include_all = filter(u_123, (x: Int) => true)
      assert(!contains(include_all, 0), "Filter - Include All: 0 (not in set)")
      assert(contains(include_all, 1), "Filter - Include All: 1")
      assert(contains(include_all, 2), "Filter - Include All: 2")
      assert(contains(include_all, 3), "Filter - Include All: 3")
      assert(!contains(include_all, 4), "Filter - Include All: 4 (not in set)")
      
      val exclude_all = filter(u_123, (x: Int) => false)
      assert(!contains(exclude_all, 0), "Filter - Exclude All: 0")
      assert(!contains(exclude_all, 1), "Filter - Exclude All: 1")
      assert(!contains(exclude_all, 2), "Filter - Exclude All: 2")
      assert(!contains(exclude_all, 3), "Filter - Exclude All: 3")
      assert(!contains(exclude_all, 4), "Filter - Exclude All: 4")

      val geq_2 = filter(u_123, (x: Int) => x >= 2)
      assert(!contains(geq_2, 0), "Filter - Greater or equal 2: 0 (not in set)")
      assert(!contains(geq_2, 1), "Filter - Greater or equal 2: 1")
      assert(contains(geq_2, 2), "Filter - Greater or equal 2: 2")
      assert(contains(geq_2, 3), "Filter - Greater or equal 2: 3")
      assert(!contains(geq_2, 4), "Filter - Greater or equal 2: 4 (not in set)")
  }

  test("forall returns true when predicate is true for all elements in the set") {
    new TestSets:
      val u_123 = union(union(s1, s2), s3)
      
      assert(forall(u_123, (x: Int) => true), "true for all of {1, 2, 3}")
      assert(!forall(u_123, (x: Int) => false), "false for all of {1, 2, 3}")
      assert(!forall(u_123, (x: Int) => x >= 2), "only accepts x >= 2, so should be false")
  }

    test("exists returns true when at least one of the element in the set is accepted by the predicate") {
    new TestSets:
      val u_123 = union(union(s1, s2), s3)
      
      assert(exists(u_123, (x: Int) => true), "true for all of {1, 2, 3}")
      assert(!exists(u_123, (x: Int) => false), "false for all of {1, 2, 3}")
      assert(exists(u_123, (x: Int) => x >= 2), "x >= 2 for {1, 2, 3}")
      assert(exists(u_123, (x: Int) => x == 2), "x == 2 for {1, 2, 3}")
      assert(!exists(u_123, (x: Int) => x == 4), "x == 4 for {1, 2, 3}, out of range")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
