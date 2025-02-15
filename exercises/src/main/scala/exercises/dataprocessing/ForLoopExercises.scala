package exercises.dataprocessing

object ForLoopExercises {

//  def sum(numbers: List[Int]): Int = {
//    var total = 0
//
//    for (number <- numbers)
//      total += number
//
//    total
//  }
  def sum(numbers: List[Int]): Int =
    pattern(numbers, 0)(_ + _)

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  //  def size[A](items: List[A]): Int = {
  //    var size = 0
  //    for (_ <- items)
  //      size += 1
  //    size
  //  }
  def size[A](items: List[A]): Int =
    pattern[A, Int](items, 0)((_, b) => b + 1)

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  //  def min(numbers: List[Int]): Option[Int] = {
  //    var min = numbers.headOption
  //    for (n <- numbers)
  //      if (n < min.get)
  //        min = Some(n)
  //    min
  //  }
  def min(numbers: List[Int]): Option[Int] =
    pattern[Int, Option[Int]](numbers, Option.empty[Int]) {
      case (a, None)    => Some(a)
      case (a, Some(m)) => Some(a min m)
    }

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  //  def wordCount(words: List[String]): Map[String, Int] = {
  //    var frequencies: Map[String, Int] = Map.empty
  //
  //    for (w <- words)
  //      frequencies.get(w) match {
  //        case None    => frequencies = frequencies.updated(w, 1)
  //        case Some(v) => frequencies = frequencies.updated(w, v + 1)
  //      }
  //
  //    frequencies
  //  }
  def wordCount(words: List[String]): Map[String, Int] =
    pattern(words, Map.empty[String, Int]) { (w, frequencies) =>
      frequencies.get(w) match {
        case None    => frequencies.updated(w, 1)
        case Some(v) => frequencies.updated(w, v + 1)
      }
    }

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def pattern[A, B](as: Iterable[A], z: B)(f: (A, B) => B): B = {
    var b = z
    for (a <- as) b = f(a, b)
    b
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    elements.foldLeft(List.empty[To])((to, from) => to :+ update(from))

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    elements.foldLeft(List.empty[A])((as, a) => a :: as)

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    elements.foldLeft(Option.empty[A])((_, a) => Some(a))

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(implicit ord: Ordering[A]): Option[A] =
    elements.foldLeft(Option.empty[A]) {
      case (None, element)    => Some(element)
      case (Some(a), element) => Some(ord.min(a, element))
    }
}
