package exercises.dataprocessing

import scala.math

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] = {

    implicit val sampleOrdering: Ordering[Sample] = (x: Sample, y: Sample) =>
      (x.temperatureFahrenheit - y.temperatureFahrenheit).toInt

    val minSamples = samples.partitions.map(_.min)
    minSamples.minOption
  }

  def minTemperatureWithFold(samples: ParList[Sample]): Option[Double] = {

    def combine(currValue: Option[Double], sample: Sample): Option[Double] =
      (currValue, sample) match {
        case (None, sample: Sample)                                              => Some(sample.temperatureFahrenheit)
        case (Some(minValue), sample) if sample.temperatureFahrenheit < minValue => Some(sample.temperatureFahrenheit)

      }

    def combineIntermediate(currValue: Option[Double], intermediateValue: Option[Double]) =
      (currValue, intermediateValue) match {
        case (None, maybeV2)                         => maybeV2
        case (maybeV1, None)                         => maybeV1
        case (Some(v1: Double), Some(v2)) if v1 < v2 => Some(v1)
        case (Some(v1), Some(v2: Double)) if v2 < v1 => Some(v2)
      }

    val default = Option.empty[Double]
    samples.partitions.map((p: List[Sample]) => p.foldLeft(default)(combine)).foldLeft(default)(combineIntermediate)

  }

  def minSampleWithFoldLeft(samples: ParList[Sample]): Option[Sample] = {
    def combine(currValue: Option[Sample], sample: Sample): Option[Sample] =
      (currValue, sample) match {
        case (None, sample: Sample) => Some(sample)
        case (Some(currMinSample), sample) if sample.temperatureFahrenheit < currMinSample.temperatureFahrenheit =>
          Some(sample)
        case (currValue, _) => currValue

      }

    def combineIntermediate(currValue: Option[Sample], intermediateValue: Option[Sample]): Option[Sample] =
      (currValue, intermediateValue) match {
        case (None, maybeV2)                                                                             => maybeV2
        case (maybeV1, None)                                                                             => maybeV1
        case (Some(v1: Sample), Some(v2: Sample)) if v1.temperatureFahrenheit < v2.temperatureFahrenheit => Some(v1)
        case (Some(v1: Sample), Some(v2: Sample)) if v2.temperatureFahrenheit < v1.temperatureFahrenheit => Some(v2)

      }

    val default = Option.empty[Sample]
    samples.foldLeft(default)(combine)(combineIntermediate)
  }

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def sumAndSizeTemperature(samples: ParList[Sample]): Option[(Double, Int)] = {
    def getSumOAndSizefTemperatureFromSampleList(sampleList: List[Sample]): (Double, Int) =
      sampleList.foldLeft((0.0, 0)) { case ((sum, size), sample) => (sum + sample.temperatureFahrenheit, size + 1) }
    def sumTuples(tuples: List[(Double, Int)]): (Double, Int) = tuples.foldLeft((0.0, 0)) {
      case ((sum, size), (sum1, size1)) =>
        (sum + sum1, size + size1)
    }

    samples.partitions match {
      case list =>
        Some(
          sumTuples(
            list
              .map(getSumOAndSizefTemperatureFromSampleList)
          )
        )
      case Nil => None
    }

  }

  def sizeSample(samples: ParList[Sample]): Int =
    samples.partitions.foldLeft(0)(_ + _.size)

  def sizeSampleWithFoldLeft(samples: ParList[Sample]): Int = samples.foldLeft(0)((acc, _) => acc + 1)(_ + _)

  def averageTemperature(samples: ParList[Sample]): Option[Double] =
    sumAndSizeTemperature(samples) match {
      case Some((sum, size)) if size > 0 => Some(sum / size)
      case _                             => None
    }

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  def foldLeft[From, To](parList: ParList[From], default: To)(combine: (To, From) => To)(
    combineIntermediate: (To, To) => To
  ): To =
    parList.partitions.map(partition => partition.foldLeft(default)(combine)).foldLeft(default)(combineIntermediate)

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  def monoFoldLeft[A](parList: ParList[A], default: A)(combine: (A, A) => A): A =
    parList.partitions.map(_.foldLeft(default)(combine)).foldLeft(default)(combine)

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = ???,
      max = ???,
      sum = ???,
      size = ???
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    ???
}
