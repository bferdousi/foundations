package exercises.dataprocessing

object ForLoopExercises {

  def sum(numbers: List[Int]): Int = {
    var total = 0

    for (number <- numbers)
      total += number

    total
  }

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int =
    foldLeft(0)(items)((total, _) => total + 1)

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] =
    foldLeft(Option.empty[Int])(numbers)(
      {
        case (Some(minNumberSoFar), number) => Some(number min minNumberSoFar)
        case (None, number)                 => Some(number)
      }
    )

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] =
    foldLeft(Map.empty[String, Int])(words)((prev, item) => prev.updated(item, prev.getOrElse(item, 0) + 1))

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def foldLeft[A, B](initialValue: A)(list: List[B])(update: (A, B) => A): A = {
    var v = initialValue
    list.foreach(item => v = update(v, item))
    v
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    elements.foldLeft(List.empty[To])((updatedList, item) => updatedList :+ update(item))

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    elements.foldLeft(List.empty[A])((reversedList, item) => item +: reversedList)

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    elements.foldLeft(Option.empty[A])((_, item) => Some(item))
  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(ord: Ordering[A]): Option[A] =
    elements.foldLeft(None: Option[A])({
      case (Some(v), item) => Some(ord.min(v, item))
      case (None, item)    => Some(item)
    })

}
