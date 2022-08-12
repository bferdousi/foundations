package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]]) {
  def foldLeft[To](default: To)(combine: (To, A) => To)(
    combineIntermediate: (To, To) => To
  ): To =
    this.partitions.map(partition => partition.foldLeft(default)(combine)).foldLeft(default)(combineIntermediate)

  def monoFoldLeft(param: Monoid[A]): A =
    this.partitions.map(_.foldLeft(param.default)(param.combine)).foldLeft(param.default)(param.combine)

  def toList =
    this.partitions.flatten

  def map[B](func: A => B): ParList[B] = ParList(this.partitions.map(l => l.map(func)))

  def size: Int = this.mapReduce(_ => 1)(monoid = Monoid.sumInt)

  def mapReduce[To](update: A => To)(monoid: Monoid[To]): To = map(update).monoFoldLeft(monoid)
}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A]): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)

}
