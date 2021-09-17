package exercises.dataprocessing

import exercises.dataprocessing.TemperatureExercises._
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

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

    assert(minSampleByTemperature(parSamples).contains(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1)))
  }

  test("minSampleByTemperature is consistent with List min") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(_.temperatureFahrenheit))
    }
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
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

    assert(averageTemperature(parSamples).contains(53.6))
  }

  test("averageTemperature PBT") {
    forAll { (samples: ParList[Sample]) =>
      averageTemperature(samples) match {
        case None => assert(samples.isEmpty)
        case Some(average) =>
          val newSamples = samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
          averageTemperature(newSamples) match {
            case None            => fail("Problem with map")
            case Some(doubleAvg) => assert((doubleAvg - average * 2).abs < 0.00001)
          }
      }
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

  test("monoFoldLeft sum PBT") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(0)(_ + _) == numbers.toList.sum)
    }
  }

  def checkMonoid[A: Arbitrary](name: String, param: Monoid[A]): Unit = {
    test(s"MonoFoldParam $name - combine to be a no-op with default") {
      forAll {(value: A) =>
        assert(param.combine(value, param.default) == value)
        assert(param.combine(param.default, value) == value)
      }
    }

    test(s"MonoFoldParam $name - combine is associative") {
      forAll { (first: A, second: A, third: A) =>
        val oneWay = param.combine(param.combine(first, second), third)
        val otherWay = param.combine(first, param.combine(second, third))
      }
    }
  }

  checkMonoid("sumInt", Monoid.sumInt)
}
