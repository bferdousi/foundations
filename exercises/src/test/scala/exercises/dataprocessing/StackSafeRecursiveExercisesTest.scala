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
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
    assert(min(List.fill(largeSize)(0)) == Some(0))
  }

  test("min is consistent with min library") {
    forAll { (list: List[Int]) =>
      assert(min(list) == list.minOption)
    }
  }

  test("reverse") {
    assert(reverse(List(2, 5, 1, 8)) == List(8, 1, 5, 2))
    assert(reverse(Nil) == Nil)
    val largeSizeList = List.fill(largeSize)(0)
    assert(largeSizeList == reverse(largeSizeList))
  }

  test("reverse is same as std reverse") {
    forAll { (list: List[Int]) =>
      assert(reverse(list) == list.reverse)
    }
  }

  test("foldLeft") {
    assert(foldLeft(List.fill(largeSize)(1), 0)(_ + _) == largeSize)
  }

  test("foldLeft is same as std") {
    forAll { (list: List[Int], default: Int, func: (Int, Int) => Int) =>
      assert(foldLeft(list, default)(func) == list.foldLeft(default)(func))
    }
  }

}
