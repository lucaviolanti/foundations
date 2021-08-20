package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size and concat") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      assert(size(l1 ++ l2) == size(l1) + size(l2))
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min returns a number that is less or equal than all other numbers") {
    forAll { (l: List[Int]) =>
      min(l) match {
        case Some(value) => l.foreach(v => assert(value <= v))
        case None        => assert(l.isEmpty)
      }
    }
  }

  test("min returns a number that is contained in the input list") {
    forAll { (l: List[Int]) =>
      min(l) match {
        case Some(value) => assert(l.contains(value))
        case None        => assert(l.isEmpty)
      }
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount returns frequencies > 0") {
    forAll { (words: List[String]) =>
      wordCount(words).values.foreach(f => assert(f > 0))
    }
  }

  test("pattern/foldLeft processes items in order") {
    forAll { (numbers: List[Int]) =>
      assert(pattern(numbers, List.empty[Int])((number, prefix) => prefix :+ number) == numbers)
    }
  }
}
