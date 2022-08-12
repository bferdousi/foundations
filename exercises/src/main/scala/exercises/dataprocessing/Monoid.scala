package exercises.dataprocessing

trait Monoid[A] {

  def default: A;

  def combine: (A, A) => A

}

object Monoid {

  object sumInt extends Monoid[Int] {
    override def default: Int = 0

    override def combine: (Int, Int) => Int = _ + _
  }

  object sumDouble extends Monoid[Double] {
    override def default: Double = 0.0

    override def combine: (Double, Double) => Double = _ + _
  }

  object sumDoubleInt extends Monoid[(Double, Int)] {
    override def default: (Double, Int) = (sumDouble.default, sumInt.default)

    override def combine: ((Double, Int), (Double, Int)) => (Double, Int) = (v1, v2) =>
      (sumDouble.combine(v1._1, v2._1), sumInt.combine(v1._2, v2._2))
  }

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def default: (A, B) = (monoidA.default, monoidB.default)

      override def combine: ((A, B), (A, B)) => (A, B) = (first, second) =>
        (monoidA.combine(first._1, second._1), monoidB.combine(first._2, second._2))
    }

  object minDouble extends Monoid[Option[Double]] {
    override def default: Option[Double] = None

    override def combine: (Option[Double], Option[Double]) => Option[Double] = (first, second) =>
      (first, second) match {
        case (Some(v1), Some(v2)) => Some(v1.min(v2))
        case (Some(value), None)  => Some(value)
        case (None, Some(value))  => Some(value)
      }
  }

  object minTemperatureSample extends Monoid[Option[Sample]] {
    override def default: Option[Sample] = None

    override def combine: (Option[Sample], Option[Sample]) => Option[Sample] = (first, second) =>
      (first, second) match {
        case (Some(v1), Some(v2)) if v1.temperatureFahrenheit < v2.temperatureFahrenheit => Some(v1)
        case (Some(_), Some(v2))                                                         => Some(v2)
        case (v1, None)                                                                  => v1
        case (None, v2)                                                                  => v2
      }
  }

}
