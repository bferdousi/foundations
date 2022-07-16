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

  test("min property based simpler") {
    forAll { (list1: List[Int]) =>
      val minNumber = min(list1)
      minNumber match {
        case Some(minNumberValue) => for (number <- list1) assert(minNumberValue <= number)
        case None                 => assert(list1.isEmpty)
      }
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount property based on counts all the word occurance and contains the word") {
    forAll { (l: List[String]) =>
      val output = wordCount(l)
      for {
        word <- output.keys
      } assert(l.count(_ == word) == output.getOrElse(word, 0) && l.contains(word))
    }

  }
}
