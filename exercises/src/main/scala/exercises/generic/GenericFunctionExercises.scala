package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

object GenericFunctionExercises {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  val names: Pair[String] = Pair("John", "Elisabeth")
  val ages: Pair[Int]     = Pair(32, 46)

  case class Pair[A](first: A, second: A) {
    // 1a. Implement `swap` which exchanges `first` and `second`
    // such as Pair("John", "Doe").swap == Pair("Doe", "John")
    def swap: Pair[A] = Pair(second, first)

    // 1b. Implement `map` which applies a function to `first` and `second`
    // such as Pair("John", "Doe").map(_.length) == Pair(4,3)
    def map[To](update: A => To): Pair[To] = Pair(update(first), update(second))

    // 1c. Implement `zipWith` which merges two Pairs using a `combine` function
    // such as Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6)
    //         Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World ")
    // Bonus: Why did we separate the arguments of `zipWith` into two set of parentheses?
    def zipWith[Other, To](other: Pair[Other])(combine: (A, Other) => To): Pair[To] =
      Pair(combine(first, other.first), combine(second, other.second))
  }

  // 1d. Use the Pair API to decode the content of `secret`.
  // Note: You can transform a Byte into a Char using `byte.toChar`
  //       or you can create a String from an Array[Byte] using `new String(byteArray)`
  // Note: You can remove the lazy keyword from `decoded` once you have implemented it.
  val secret: Pair[List[Byte]] =
    Pair(
      first = List(103, 110, 105, 109, 109, 97, 114, 103, 111, 114, 80),
      second = List(108, 97, 110, 111, 105, 116, 99, 110, 117, 70)
    )
  val decoded: Pair[String] = secret
    .map(bs => new String(bs.toArray.map(_.toChar)))
    .map(_.reverse)
    .swap

  // 1e. Use the Pair API to combine `productNames` and `productPrices` into `products`
  // such as products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99))
  case class Product(name: String, price: Double)

  val productNames: Pair[String]  = Pair("Coffee", "Plane ticket")
  val productPrices: Pair[Double] = Pair(2.5, 329.99)

  lazy val products: Pair[Product] =
    productNames.zipWith(productPrices)(Product)

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 1f. Can you implement a method on `Pair` similar to `zipWith`, but that combines 3
  // Pairs instead of 2? If yes, can you implement this method using `zipWith`?
  // Note: Libraries often call this method `map3` and `zipWith` is often called `map2`

  object PairSyntax {
    implicit class PairOps[A, B, C, D](pa: Pair[A]) {
      def map3(pb: Pair[B], pc: Pair[C])(f: (A, B, C) => D): Pair[D] = {
        val fabcd             = (a: A, b: B) => (c: C) => f(a, b, c)
        val pcd: Pair[C => D] = pa.zipWith(pb)(fabcd)
        pcd.zipWith(pc)((fcd: C => D, c: C) => fcd(c))
      }
    }
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  val isPositive: Predicate[Int] =
    Predicate((number: Int) => number >= 0)

  val isEven: Predicate[Int] =
    Predicate((number: Int) => number % 2 == 0)

  lazy val isOddPositive: Predicate[Int] =
    isEven.flip && isPositive

  case class Predicate[A](eval: A => Boolean) {
    // DSL to call a predicate like a function
    // isPositive(10) instead of isPositive.eval(10)
    def apply(value: A): Boolean = eval(value)

    // 2a. Implement `&&` that combines two predicates using logical and
    // such as (isEven && isPositive)(12) == true
    // but     (isEven && isPositive)(11) == false
    //         (isEven && isPositive)(-4) == false
    //         (isEven && isPositive)(-7) == false
    def &&(other: Predicate[A]): Predicate[A] =
      Predicate((a: A) => this(a) && other(a))

    // 2b. Implement `||` that combines two predicates using logical or
    // such as (isEven || isPositive)(12) == true
    //         (isEven || isPositive)(11) == true
    //         (isEven || isPositive)(-4) == true
    // but     (isEven || isPositive)(-7) == false
    def ||(other: Predicate[A]): Predicate[A] =
      Predicate((a: A) => this(a) || other(a))

    // 2c. Implement `flip` that reverses a predicate
    // such as isEven.flip(11) == true
    def flip: Predicate[A] =
      Predicate((a: A) => !this(a))

    def contramap[To](zoom: To => A): Predicate[To] =
      Predicate(user => eval(zoom(user)))
  }

  object Predicate {
    def False[A]: Predicate[A] = Predicate(_ => false)
    def True[A]: Predicate[A]  = Predicate(_ => true)
  }

  // 2d. Implement `isValidUser`, a predicate which checks if a `User` is:
  // * an adult (older than 18 year) and
  // * their name is longer than or equal to 3 characters and
  // * their name is capitalized, meaning that it starts with an uppercase letter
  // such as isValidUser(User("John", 20)) == true
  // but     isValidUser(User("John", 17)) == false // user is not an adult
  //         isValidUser(User("john", 20)) == false // name is not capitalized
  //         isValidUser(User("x"   , 23)) == false // name is too small
  // Note: Can you use methods from the Predicate API such as `&&`, `||` or `flip`?
  // You may want to create new Predicate methods to improve the implementation of `isValidUser`.
  case class User(name: String, age: Int)

  lazy val isValidUser: Predicate[User] =
    byUser(_.age)(isOlderThan(18)) &&
      byUser(_.name)(isLongerThan(3) && isCapitalised)

  def byUser[To](zoom: User => To)(subPredicate: Predicate[To]): Predicate[User] =
    subPredicate.contramap(zoom)
  def isOlderThan(age: Int): Predicate[Int]   = Predicate(_ >= age)
  def isLongerThan(l: Int): Predicate[String] = Predicate(_.length >= l)
  val isCapitalised: Predicate[String]        = Predicate(s => s == s.capitalize)

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  // very basic representation of JSON
  type Json = String

  trait JsonDecoder[A] { self =>
    def decode(json: Json): A
    def map[To](update: A => To): JsonDecoder[To] =
      new JsonDecoder[To] {
        override def decode(json: Json): To = update(self.decode(json))
      }
    def orElse(other: JsonDecoder[A]): JsonDecoder[A] = new JsonDecoder[A] {
      override def decode(json: Json): A = Try(self.decode(json)).getOrElse(other.decode(json))
    }
  }

  val intDecoder: JsonDecoder[Int] = new JsonDecoder[Int] {
    def decode(json: Json): Int = json.toInt
  }

  val stringDecoder: JsonDecoder[String] = new JsonDecoder[String] {
    def decode(json: Json): String =
      if (json.startsWith("\"") && json.endsWith("\"")) // check it starts and ends with `"`
        json.substring(1, json.length - 1)
      else
        throw new IllegalArgumentException(s"$json is not a valid JSON string")
  }

  // SAM syntax for JsonDecoder
  val intDecoderSAM: JsonDecoder[Int] =
    (json: Json) => json.toInt

  // 3a. Implement `userIdDecoder`, a `JsonDecoder` for the `UserId` case class
  // such as userIdDecoder.decode("1234") == UserId(1234)
  // but     userIdDecoder.decode("hello") would throw an Exception
  case class UserId(value: Int)
  lazy val userIdDecoder: JsonDecoder[UserId] = new JsonDecoder[UserId] {
    override def decode(json: Json): UserId = UserId(intDecoder.decode(json))
  }

  lazy val userIdDecoderUsingMap: JsonDecoder[UserId]            = map(intDecoder)(UserId)
  lazy val userIdDecoderUsingMapInsideTrait: JsonDecoder[UserId] = intDecoder.map(UserId)

  // 3b. Implement `localDateDecoder`, a `JsonDecoder` for `LocalDate`
  // such as localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // but     localDateDecoder.decode("2020-03-26") would throw an Exception
  // and     localDateDecoder.decode("hello") would throw an Exception
  // Note: You can parse a `LocalDate` using `LocalDate.parse` with a java.time.format.DateTimeFormatter
  // e.g. DateTimeFormatter.ISO_LOCAL_DATE
  lazy val localDateDecoder: JsonDecoder[LocalDate] = new JsonDecoder[LocalDate] {
    override def decode(json: Json): LocalDate =
      LocalDate.parse(stringDecoder.decode(json), DateTimeFormatter.ISO_LOCAL_DATE)
  }

  lazy val localDateDecoderUsingMap: JsonDecoder[LocalDate] =
    map(stringDecoder)(LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE))

  lazy val localDateDecoderUsingMapInsideTrait: JsonDecoder[LocalDate] =
    stringDecoder.map(LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE))

  // 3c. Implement `map` a generic function that converts a `JsonDecoder` of `From`
  // into a `JsonDecoder` of `To`.
  // Bonus: Can you re-implement `userIdDecoder` and `localDateDecoder` using `map`
  def map[From, To](decoder: JsonDecoder[From])(update: From => To): JsonDecoder[To] =
    new JsonDecoder[To] {
      override def decode(json: Json): To = update(decoder.decode(json))
    }

  // 3d. Move `map` inside of `JsonDecoder` trait so that we can use the syntax
  // `intDecoder.map(_ + 1)` instead of `map(intDecoder)(_ + 1)`

  // 3e. Imagine we have to integrate with a weird JSON API where dates are sometimes encoded
  // using a String with the format "yyyy-mm-dd" and sometimes they are encoded using
  // JSON numbers representing the number of days since the epoch. For example,
  // weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26)
  // but weirdLocalDateDecoder.decode("hello") would throw an Exception
  // Try to think how we could extend JsonDecoder so that we can easily implement
  // other decoders that follow the same pattern.
  lazy val weirdLocalDateDecoder: JsonDecoder[LocalDate] = {
//  new JsonDecoder[LocalDate] {
//    override def decode(json: Json): LocalDate = {
//      val firstTry                   = Try(localDateDecoderUsingMapInsideTrait.decode(json))
//      lazy val daysSinceEpochDecoder = LocalDate.ofEpochDay(json.toLong)
//
//      firstTry.getOrElse(daysSinceEpochDecoder)
//    }

//    (json: Json) =>
//      Try(localDateDecoderUsingMapInsideTrait.decode(json)).getOrElse(LocalDate.ofEpochDay(json.toLong))

    val longJsonDecoder: JsonDecoder[Long]           = _.toLong
    val longLocalDateDecoder: JsonDecoder[LocalDate] = longJsonDecoder.map(LocalDate.ofEpochDay)
    localDateDecoder orElse longLocalDateDecoder
  }

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 3f. How would you define and implement a `JsonDecoder` for a generic `Option`?
  // such as we can decode:
  // * "1" into a Some(1)
  // * "\"2020-08-03\"" into a Some(LocalDate.of(2020,08,3))
  // * "\"null\"" into a Some("null")
  // * "null" into "None"
  // Note: you may need to change the function signature
  def optionDecoder[A]: JsonDecoder[Option[A]] =
    ???

  // 3g. `JsonDecoder` currently throws an exception if the input is not a valid JSON.
  // How could you change the API so that it doesn't happen anymore?
}
