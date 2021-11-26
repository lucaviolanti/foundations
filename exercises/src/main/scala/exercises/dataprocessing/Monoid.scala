package exercises.dataprocessing

/** Please use a default such as when used with combine it is a no-operation */
trait Monoid[A] {
  val default: A
  def combine(first: A, second: A): A
}

object Monoid {
  val sumInt: Monoid[Int] = new Monoid[Int] {
    override val default: Int = 0

    override def combine(first: Int, second: Int): Int = first + second
  }

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    override val default: Double = 0.0

    override def combine(first: Double, second: Double): Double = first + second
  }

  val sumDoubleInt: Monoid[(Double, Int)] = zip(sumDouble, sumInt)

  def zip[A, B](mA: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override val default: (A, B) = (mA.default, mb.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) =
      (mA.combine(first._1, second._1), mb.combine(first._2, second._2))
  }

  val minSample: Monoid[Option[Sample]] =
    compareSample((s1, s2) => if (s1.temperatureFahrenheit < s2.temperatureFahrenheit) s1 else s2)

  val maxSample: Monoid[Option[Sample]] =
    compareSample((s1, s2) => if (s1.temperatureFahrenheit > s2.temperatureFahrenheit) s1 else s2)

  def compareSample(compare: (Sample, Sample) => Sample): Monoid[Option[Sample]] = new Monoid[Option[Sample]] {
    override val default: Option[Sample] = None

    override def combine(first: Option[Sample], second: Option[Sample]): Option[Sample] =
      (first, second) match {
        case (None, None)         => None
        case (Some(s), None)      => Some(s)
        case (None, Some(s))      => Some(s)
        case (Some(s1), Some(s2)) => Some(compare(s1, s2))
      }
  }
}
