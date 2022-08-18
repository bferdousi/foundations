package exercises.dataprocessing

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double,         // sum of all temperatures in Fahrenheit
  size: Int            // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"sum = $sum\n" +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}

object SummaryMonoid extends Monoid[Summary] {
  override def default: Summary = Summary(None, None, 0.0, 0)

  override def combine: (Summary, Summary) => Summary = (first, second) =>
    Summary(
      Monoid.minTemperatureSampleMonoid.combine(first.min, second.min),
      Monoid.maxTemperatureSampleMonoid.combine(first.max, second.max),
      first.sum + second.sum,
      Monoid.sumInt.combine(first.size, second.size)
    )
}
