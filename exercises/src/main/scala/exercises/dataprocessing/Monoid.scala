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
}
