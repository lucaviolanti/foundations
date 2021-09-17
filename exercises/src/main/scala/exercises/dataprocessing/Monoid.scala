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
}
