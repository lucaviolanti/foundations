package exercises.dataprocessing

import exercises.dataprocessing.StackSafeRecursiveExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class StackSafeRecursiveExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  val largeSize = 100000

  test("unsafeSum is not stack-safe") {
    try {
      unsafeSum(List.fill(largeSize)(0))
      fail("Expected stack overflow")
    } catch {
      case _: StackOverflowError => succeed
      case e: Throwable          => fail(e)
    }
  }

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
    assert(sum(List.fill(largeSize)(0)) == 0)
  }

  test("sum is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)).contains(1))
    assert(min(Nil).isEmpty)
  }

  test("min is consistent with minOption") {
    forAll { (numbers: List[Int]) =>
      assert(min(numbers) == numbers.minOption)
    }
  }

  test("reverse") {
    assert(reverse(List(2, 5, 1, 8)) == List(8, 1, 5, 2))
    assert(reverse(Nil) == Nil)
  }

  test("reverse is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(reverse(numbers) == numbers.reverse)
    }
  }

  test("foldLeft") {
    val largeList = List.range(0, 100000)
    assert(foldLeft(largeList, 0)(_ + _) == largeList.foldLeft(0)(_ + _))
  }

  test("foldLeft is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(foldLeft(numbers, 0)(_ + _) == numbers.foldLeft(0)(_ + _))
    }
  }
}
