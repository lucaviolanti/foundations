package exercises.generic

import exercises.generic.GenericFunctionExercises.PairSyntax._
import exercises.generic.GenericFunctionExercises.Predicate._
import exercises.generic.GenericFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair("John", "Doe").swap == Pair("Doe", "John"))
  }

  test("Pair map") {
    assert(Pair("John", "Doe").map(_.length) == Pair(4, 3))
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
    val replicate: (Int, String) => String = (i: Int, s: String) => s * i
    assert(Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World "))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  test("Map3 works") {
    assert(
      decoded.map3(products, productPrices)((dec: String, prod: Product, price: Double) =>
        s"$dec ${prod.name} = £${price}"
      ) == Pair("Functional Coffee = £2.5", "Programming Plane ticket = £329.99")
    )
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate && PBT") {
    forAll { (f1: Int => Boolean, v: Int) =>
      val p1 = Predicate(f1)

      assert((p1 && True)(v) == p1(v))
      assert(!(p1 && False)(v))
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12))
    assert((isEven || isPositive)(11))
    assert((isEven || isPositive)(-4))
    assert(!(isEven || isPositive)(-7))
  }

  test("Predicate || PBT") {
    forAll { (f1: Int => Boolean, v: Int) =>
      val p1 = Predicate(f1)

      assert((p1 || True)(v))
      assert((p1 || False)(v) == p1(v))
    }
  }

  test("Predicate flip") {
    assert(isEven.flip(11))
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("x", 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {}

  test("JsonDecoder LocalDate") {}

  test("JsonDecoder weirdLocalDateDecoder") {}

}
