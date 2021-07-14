package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("secret examples") {
    forAll { (text: String) =>
      val len = text.length
      assert(secret(text) equals ("*" * len))
    }
  }

  test("empty secret") {
    assert(secret("") equals "")
  }

  test("valid alphanum characters for usernames are allowed") {
    forAll(Gen.alphaNumChar) { c =>
      assert(isValidUsernameCharacter(c))
    }
  }

  test("valid special characters for usernames are allowed") {
    val specialCharacters = Gen.oneOf('-', '_')
    forAll(specialCharacters) { c =>
      assert(isValidUsernameCharacter(c))
    }
  }

  test("invalid characters for usernames are rejected") {
    val specials = "§±!@£$%^&*()+[{]};:\'\"\\|,<.>/?¡€¢∞¶•ªº“‘…æ«≤≥ ".toSeq
    forAll(Gen.oneOf(specials)) { c =>
      assert(!isValidUsernameCharacter(c))
    }
  }

  test("usernames with valid characters are accepted") {
    forAll(Gen.alphaNumStr) { s =>
      assert(isValidUsername(s))
    }
  }

  test("usernames with invalid characters are rejected") {
    val specials = "§±!@£$%^&*()+[{]};:\'\"\\|,<.>/?¡€¢∞¶•ªº“‘…æ«≤≥ ".toSeq
    forAll(Gen.oneOf(specials), Gen.alphaNumStr) { (c, s) =>
      assert(!isValidUsername(c + s))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("is positive works") {
    assert(Point(2, 4, 9).isPositive)
    assert(Point(0, 0, 0).isPositive)
    assert(!Point(0, -2, 1).isPositive)
  }

  test("is even works") {
    assert(Point(2, 4, 8).isEven)
    assert(Point(0, -8, -2).isEven)
    assert(!Point(3, -2, 0).isEven)
  }

  test("forAll works") {
    assert(Point(1, 1, 1).forAll(_ == 1))
    assert(!Point(1, 2, 5).forAll(_ == 1))
  }
}
