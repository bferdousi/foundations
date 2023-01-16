package exercises.action.imperative

import exercises.action.DateGenerator._
import exercises.action.imperative.UserCreationExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Instant, LocalDate}
import scala.collection.mutable.ListBuffer
import scala.util.Try

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("parse Y/N parses Yes") {
    assert(parseYesOrNo("Y"))
  }
  test("parse Y/N parses No") {
    assert(!parseYesOrNo("N"))
  }
  test("parse Y/N throws error for invalid input") {
    assertThrows[IllegalArgumentException](parseYesOrNo("Nope"))
  }

  test("readSubscribeToMailingList example") {
    forAll { (value: Boolean) =>
      val yesOrNo = if (value) "Y" else "N"
      val inputs  = ListBuffer(yesOrNo)
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result  = readSubscribeToMailingList(console)

      assert(result == value)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }

  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result  = Try(readSubscribeToMailingList(console))

    assert(result.isFailure)
  }

  test("readDateOfBirth example success") {
    forAll { (date: LocalDate) =>
      val console = Console.mock(ListBuffer(date.format(dateOfBirthFormatter)), ListBuffer())
      val result  = readDateOfBirth(console)

      assert(result == date)
    }

  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result  = Try(readDateOfBirth(console))

    assert(result.isFailure)
  }

  test("read User property based testing") {
    forAll { (name: String, dob: LocalDate, now: Instant, yesOrNo: Boolean) =>
      val inputs   = ListBuffer(name, dob.format(dateOfBirthFormatter), if (yesOrNo) "Y" else "N")
      val outputs  = ListBuffer.empty[String]
      val console  = Console.mock(inputs, outputs)
      val clock    = Clock.constant(now)
      val result   = readUser(console, clock)
      val expected = User(name, dob, yesOrNo, clock.now())

      assert(result == expected)

    }
  }

  test("readUser example") {
    val inputs  = ListBuffer("Eda", "18-03-2001", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val clock   = Clock.constant(Instant.ofEpochSecond(123234))
    val now     = clock.now()
    val result  = readUser(console, clock)

    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribedToMailingList = true,
      createdAt = now
    )

    assert(result == expected)
  }

  //////////////////////////////////////////////
  // PART 2: Error handling
  //////////////////////////////////////////////

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readSubscribeToMailingListRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never", "N"), outputs)
    val result  = readSubscribeToMailingListRetry(console, maxAttempt = 2)

    assert(result == false)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No"""",
        "Would you like to subscribe to our mailing list? [Y/N]"
      )
    )
  }

  test("readSubscribeToMailingListRetry example invalid input") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never"), outputs)
    val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No""""
      )
    )

    // check that the error message is the same as `readSubscribeToMailingList`
    val console2 = Console.mock(ListBuffer("Never"), ListBuffer.empty[String])
    val result2  = Try(readSubscribeToMailingList(console2))
    assert(result.failed.get.getMessage == result2.failed.get.getMessage)
  }

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(readDateOfBirthRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result  = readDateOfBirthRetry(console, maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs        = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console        = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result         = Try(readDateOfBirthRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

}
