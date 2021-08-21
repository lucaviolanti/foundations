package exercises.dataprocessing

import scala.annotation.tailrec

object StackSafeRecursiveExercises {

  def unsafeSum(numbers: List[Int]): Int =
    numbers match {
      case Nil          => 0
      case head :: tail => head + unsafeSum(tail)
    }

  def sum(numbers: List[Int]): Int = {
    @tailrec
    def go(numbers: List[Int], accumulator: Int): Int =
      numbers match {
        case Nil          => accumulator
        case head :: tail => go(tail, accumulator + head)
      }
    go(numbers, 0)
  }

  // a. Implement `min` using a recursion
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  def min(numbers: List[Int]): Option[Int] = {
    @tailrec
    def go(numbers: List[Int], current: Int): Option[Int] =
      numbers match {
        case Nil => Some(current)
        case head :: tail =>
          if (head < current) go(tail, head)
          else go(tail, current)
      }

    numbers match {
      case Nil => None
      case _   => go(numbers, Int.MaxValue)
    }
  }

  // b. Implement `reverse` using a recursion
  // such as reverse(List(2,5,1,8)) == List(8,1,5,2)
  // and     reverse(Nil) == Nil
  // Note: Ensure size is stack-safe
  def reverse[A](items: List[A]): List[A] = {
    @tailrec
    def go(items: List[A], done: List[A]): List[A] =
      items match {
        case Nil          => done
        case head :: tail => go(tail, head :: done)
      }

    go(items, List.empty[A])
  }

  // c. Implement `foldLeft` using a recursion
  // Note: Ensure size is stack-safe
  @tailrec
  def foldLeft[From, To](items: List[From], default: To)(combine: (To, From) => To): To =
    items match {
      case Nil          => default
      case head :: tail => foldLeft(tail, combine(default, head))(combine)
    }
}
