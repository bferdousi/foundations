package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {
  implicit val ec: ExecutionContext = ExecutionContext.global

  ignore("Partionsize test") {
    forAll { (listSize: Int, numberOfPartions: Int) =>
      val list = List.fill(listSize)(0)
      assert(
        ParList
          .byPartitionSize(math.ceil(listSize.toDouble / numberOfPartions).toInt, list)
          .partitions
          .length == numberOfPartions
      )
    }
  }

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("Property based test for average") {
    forAll { (samples: ParList[Sample]) =>
      val flattenned  = samples.partitions.flatten.map(_.temperatureFahrenheit)
      val expectation = if (flattenned.isEmpty) None else Some(flattenned.sum / flattenned.size)
      assert(averageTemperature(samples) == expectation)
    }
  }

  test("size test with fold") {
    forAll { (list: List[Sample]) =>
      val parList = ParList.byPartitionSize(3, list)
      assert(sizeSampleWithFoldLeft(parList) == list.size)
    }
  }

  val genDouble: Gen[Double] = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val genInt: Gen[Int]       = Gen.choose(Int.MinValue, Int.MaxValue)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  def checkMonoid[A](param: Monoid[A], gen: Gen[A], name: String) = {
    test(s"${name} is default no-op") {
      forAll(gen) { (value: A) =>
        assert(param.combine(param.default, value) == value)
        assert(param.combine(value, param.default) == value)
      }
    }
    test(s"${name} is combine associative") {
      forAll(gen, gen, gen) { (value1: A, value2: A, value3: A) =>
        assert(
          param.combine(param.combine(value1, value2), value3) == param.combine(value1, param.combine(value2, value3))
        )
      }
    }
  }
  checkMonoid(Monoid.sumInt, genInt, "sumInt")

  checkMonoid(Monoid.sumDouble, genDouble, "sumDouble")

  checkMonoid(Monoid.zip(Monoid.sumInt, Monoid.sumInt), Gen.zip(genInt, genInt), "zip")

  checkMonoid(Monoid.minTemperatureSample, Gen.option(sampleGen), "min temperature sample")

  test("sum works by monofold") {
    forAll { (parlist: ParList[Sample]) =>
      val temperatures = parlist.toList.map(_.temperatureFahrenheit)
      assert(sumTemperature(parlist) == temperatures.sum)
    }
  }

  test("size works by monofold") {
    forAll { (parlist: ParList[Sample]) =>
      assert(parlist.size == parlist.toList.size)
    }
  }

  test("foldMap is consistent with map followed by monofoldLeft") {
    forAll { (numbers: ParList[String], func: String => Int) =>
      val monoid = Monoid.sumInt
      assert(numbers.foldMap(func)(monoid) == numbers.map(func).monoFoldLeft(monoid))
    }
  }

  test("parfold map is same as fold map") {
    val pool                          = Executors.newFixedThreadPool(3)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    forAll { (numbers: ParList[String], func: String => Int) =>
      val monoid = Monoid.sumInt
      assert(numbers.parFoldMap(func)(monoid) == numbers.foldMap(func)(monoid))
    }
  }

  ignore("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
