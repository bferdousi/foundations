package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalactic.anyvals.PosInt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits has numbers") {
    forAll { (number: Int) =>
      val input = Math.abs(number).toString
      assert(selectDigits(input) == input)
    }
  }

  test("secret does not show char") {
    assert(secret("Hello1") == "******")
  }

  test("secret has all asterisk") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("isvalidName works for letters") {
    assert(isValidUsernameCharacter('A'))
    assert(isValidUsernameCharacter('n'))
    assert(isValidUsernameCharacter('3'))
    assert(isValidUsernameCharacter('-'))
    assert(isValidUsernameCharacter('_'))
    assert(!isValidUsernameCharacter('?'))
    assert(!isValidUsernameCharacter('['))
  }

  test("isValidUsername works") {
    assert(isValidUsername("john-doe"))
    assert(!isValidUsername("john*doe"))

  }

  test("isValidUsername works property") {
    forAll { (text: String) =>
      assert(isValidUsername(text) == isValidUsername(text.reverse))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point is positive works") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point iseven works") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point((x * 2).max(0), (y * 2).max(0), (x * 2).max(0)).isEven)
    }
  }

  test("for all works") {
    assert(Point(1, 1, 1).forAll(_ == 1))
    assert(!Point(1, 2, 3).forAll(_ == 1))
  }

  test("property test for all") {
    forAll { (x: Int, y: Int, z: Int, pred: Function1[Int, Boolean]) =>
      assert(Point(x, y, z).forAll(pred) == List(x, y, z).forall(pred))
    }
  }
}
