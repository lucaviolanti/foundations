package exercises.dataprocessing

import exercises.dataprocessing.TemperatureExercises._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.ExecutionContext

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
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

    assert(minSampleByTemperature(parSamples).contains(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1)))
  }

  test("minSampleByTemperature is consistent with List min") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(_.temperatureFahrenheit))
    }
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

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
    val parSamples = ParList.byPartitionSize(3, samples, ExecutionContext.global)

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

  test("size is consistent with List size") {
    forAll { (numbers: ParList[Sample]) =>
      assert(numbers.size == numbers.toList.size)
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
      assert(numbers.monoFoldLeftV1(0)(_ + _) == numbers.toList.sum)
    }
  }

  test("foldMap with identity is consistent with monoFoldLeft") {
    forAll { (numbers: ParList[Int]) =>
      val monoid = Monoid.sumInt
      // Using identity as the simplest update function
      assert(numbers.foldMap(identity)(monoid) == numbers.monoFoldLeft(monoid))
    }
  }

  test("parFoldMap is consistent with foldMap") {
    forAll { (numbers: ParList[Int]) =>
      val monoid = Monoid.sumInt
      // Using identity as the simplest update function
      assert(
        numbers.parFoldMap(identity)(monoid) == numbers.foldMap(identity)(monoid)
      )
    }
  }

  val intGen: Gen[Int]                 = Gen.choose(Int.MinValue, Int.MaxValue)
  val doubleGen: Gen[Double]           = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val doubleIntGen: Gen[(Double, Int)] = Gen.zip(doubleGen, intGen)

  // implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
  //   PropertyCheckConfiguration(minSuccessful = 10000)

  checkMonoid("sumInt", Monoid.sumInt, intGen)
  checkMonoid("sumDouble", Monoid.sumDouble, doubleGen)
  checkMonoid("sumDoubleInt", Monoid.sumDoubleInt, doubleIntGen)
  checkMonoid("zip", Monoid.zip(Monoid.sumInt, Monoid.sumInt), Gen.zip(intGen, intGen))
  checkMonoid("minSample", Monoid.minSample, Gen.option(sampleGen))

  def checkMonoid[A](name: String, param: Monoid[A], gen: Gen[A]): Unit = {
    test(s"Monoid $name - combine to be a no-op with default") {
      forAll(gen) { (value: A) =>
        assert(param.combine(value, param.default) == value)
        assert(param.combine(param.default, value) == value)
      }
    }

    test(s"Monoid $name - combine is associative") {
      forAll(gen, gen, gen) { (first: A, second: A, third: A) =>
        val oneWay   = param.combine(param.combine(first, second), third)
        val otherWay = param.combine(first, param.combine(second, third))
      }
    }
  }
}
