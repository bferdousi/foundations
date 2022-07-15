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
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size property based testing") {
    forAll { (list: List[Int]) =>
      assert(size(list) == list.size)
    }

  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min property based") {
    forAll { (list1: List[Int], list2: List[Int]) =>
      val min1 = min(list1)
      val min2 = min(list2)
      (min1, min2) match {
        case (Some(v1), Some(v2)) => assert(min(list1.concat(list2)).getOrElse(Int.MaxValue) == Math.min(v1, v2))
        case (None, _)            => assert(min(list1.concat(list2)) == min2)
        case (_, None)            => assert(min(list1.concat(list2)) == min1)
      }
    }
  }

  ignore("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

}
