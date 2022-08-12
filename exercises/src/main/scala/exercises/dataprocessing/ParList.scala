package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]])(implicit ec: ExecutionContext) {
  def foldLeft[To](default: To)(combine: (To, A) => To)(
    combineIntermediate: (To, To) => To
  ): To =
    this.partitions.map(partition => partition.foldLeft(default)(combine)).foldLeft(default)(combineIntermediate)

  def monoFoldLeft(param: Monoid[A]): A =
    this.partitions.map(_.foldLeft(param.default)(param.combine)).foldLeft(param.default)(param.combine)

  def toList =
    this.partitions.flatten

  def map[B](func: A => B): ParList[B] = ParList(this.partitions.map(l => l.map(func)))

  def size: Int = this.parFoldMap(_ => 1)(monoid = Monoid.sumInt)

  def foldMap[To](update: A => To)(monoid: Monoid[To]): To =
    this.partitions
      .map(_.foldLeft(monoid.default)((a, b) => monoid.combine(a, update(b))))
      .foldLeft(monoid.default)((a, b) => monoid.combine(a, b))

  def parFoldMap[To](update: A => To)(monoid: Monoid[To])(implicit ec: ExecutionContext): To = {

    def foldPartition(partition: List[A]): Future[To] = Future {
      val currentThread = Thread.currentThread()
      val result        = partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
      result
    }
    this.partitions
      .map(partition => foldPartition(partition))
      .map(Await.result(_, Duration.Inf))
      .foldLeft(monoid.default)(monoid.combine)
  }
}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*)(implicit ec: ExecutionContext): ParList[A] =
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
  def byPartitionSize[A](partitionSize: Int, items: List[A])(implicit ec: ExecutionContext): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)

}
