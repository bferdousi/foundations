package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
    assert(Pair("John", "Doe").swap == Pair("Doe", "John"))
  }

  test("Pair map") {
    assert(Pair("John", "Doe").map(_.length) == Pair(4, 3))
    assert(Pair(0, 1).map(identity) == Pair(0, 1))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(1, 0).zipWith(Pair(1, 0))(_ == _) == Pair(true, true))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12) == true)
    assert((isEven && isPositive)(11) == false)
    assert((isEven && isPositive)(-4) == false)
    assert((isEven && isPositive)(-7) == false)
  }

  test("Predicate && property based") {
    forAll { (v: Int) =>
      assert((isEven && isPositive)((v.abs * 2).max(0)))
    }
  }

  test("Predicate ||") {
    forAll { (eval: Int => Boolean, value: Int) =>
      val pred                  = Predicate(eval)
      def True[A]: Predicate[A] = Predicate(_ => true)

      def False[A]: Predicate[A] = Predicate(_ => false)

      assert((pred || True)(value))
      assert((pred || False)(value) == eval(value))
    }
  }

  test("Predicate flip") {}

  test("isValid user") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false) // user is not an adult
    assert(isValidUser(User("john", 20)) == false) // name is not capitalized
    assert(isValidUser(User("x", 23)) == false)    // name is too small
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assertThrows[java.lang.NumberFormatException](userIdDecoder.decode("Hello"))
  }

  test("JsonDecoder UserId Property based testing") {
    forAll { (number: Int) =>
      assert(userIdDecoder.decode(number.toString) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(Try(localDateDecoder.decode("hello")).isFailure)
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
  }

  test("JsonDecoder LocalDate property testing") {
    forAll { (localDate: LocalDate) =>
      val json = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(localDate) + "\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }
  val genLocalDate: Gen[LocalDate] =
    Gen.choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay).map(LocalDate.ofEpochDay)

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(genLocalDate)

  test("JsonDecoder weirdLocalDateDecoder") {}

}
