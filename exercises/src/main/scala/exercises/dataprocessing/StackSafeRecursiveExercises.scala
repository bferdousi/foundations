package exercises.dataprocessing

import scala.annotation.tailrec

object StackSafeRecursiveExercises {

  def unsafeSum(numbers: List[Int]): Int =
    numbers match {
      case Nil          => 0
      case head :: tail => head + unsafeSum(tail)
    }

  def sum(numbers: List[Int]): Int = {
    @tailrec
    def go(numbers: List[Int], accumulator: Int): Int =
      numbers match {
        case Nil          => accumulator
        case head :: tail => go(tail, accumulator + head)
      }
    go(numbers, 0)
  }

  // a. Implement `min` using a recursion
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  def min(numbers: List[Int]): Option[Int] = {
    def getMin(value: Int, currentMin: Option[Int]): Int = currentMin match {
      case None      => value
      case Some(min) => if (value < min) value else min
    }
    def go(numbers: List[Int], currentMin: Option[Int]): Option[Int] = numbers match {
      case ::(head, tail) => go(tail, Some(getMin(head, currentMin)))
      case Nil            => currentMin
    }
    go(numbers, None)
  }

  // b. Implement `reverse` using a recursion
  // such as reverse(List(2,5,1,8)) == List(8,1,5,2)
  // and     reverse(Nil) == Nil
  // Note: Ensure size is stack-safe
  def reverse[A](items: List[A]): List[A] = {
    def go(items: List[A], currentList: List[A]): List[A] =
      items match {
        case ::(head, tail) => go(tail, head +: currentList)
        case Nil            => currentList
      }
    go(items, List.empty)
  }

  // c. Implement `foldLeft` using a recursion
  // Note: Ensure size is stack-safe

  def foldLeft[From, To](items: List[From], default: To)(combine: (To, From) => To): To = {
    def go(items: List[From], accumulated: To): To =
      items match {
        case ::(head, tail) => go(tail, combine(accumulated, head))
        case Nil            => accumulated
      }
    go(items, default)
  }

}
