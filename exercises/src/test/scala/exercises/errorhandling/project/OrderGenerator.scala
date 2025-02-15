package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderStatus._
import org.scalacheck.Gen

import java.time.{Duration, Instant}

object OrderGenerator {

  val orderIdGen: Gen[OrderId] = Gen.alphaNumStr.map(OrderId.apply)
  val itemIdGen: Gen[ItemId]  = Gen.alphaNumStr.map(ItemId.apply)

  val instantGen: Gen[Instant] =
    for {
      seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
      nano    <- Gen.choose(0, 1000_000_000L)
    } yield Instant.ofEpochSecond(seconds, nano)

  val durationGen: Gen[Duration] =
    Gen
      .chooseNum(0L, Duration.ofDays(400).getSeconds)
      .map(Duration.ofSeconds)

  def nelOf[A](gen: Gen[A]): Gen[NEL[A]] =
    Gen.nonEmptyListOf(gen).map {
      case Nil          => sys.error("Impossible")
      case head :: tail => NEL(head, tail)
    }

  val itemGen: Gen[Item] =
    for {
      itemId   <- itemIdGen
      quantity <- Gen.chooseNum(1, 999999)
      price    <- Gen.chooseNum(0.0001, 999999999)
    } yield Item(itemId, quantity, price)

  val addressGen: Gen[Address] =
    for {
      streetNumber <- Gen.chooseNum(1, 99999)
      postCode     <- Gen.alphaNumStr
    } yield Address(streetNumber, postCode)

  val draftGen: Gen[Order] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- Gen.listOf(itemGen)
    } yield Order(orderId, Draft(items), createdAt)

  val checkoutGen: Gen[Order] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- Gen.nonEmptyListOf(itemGen).map(NEL.fromList)
      address   <- Gen.option(addressGen)
    } yield Order(orderId, Checkout(items.get, address), createdAt)

  val submittedGen: Gen[Order] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- Gen.nonEmptyListOf(itemGen).map(NEL.fromList)
      address   <- addressGen
      delay     <- durationGen
      submittedAt = createdAt.plus(delay)
    } yield Order(orderId, Submitted(items.get, address, submittedAt), createdAt)

  val deliveredGen: Gen[Order] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- Gen.nonEmptyListOf(itemGen).map(NEL.fromList)
      address   <- addressGen
      delay1    <- durationGen
      submittedAt = createdAt.plus(delay1)
      delay2 <- durationGen
      deliveredAt = submittedAt.plus(delay2)
    } yield Order(orderId, Delivered(items.get, address, submittedAt, deliveredAt), createdAt)

  val orderGen: Gen[Order] =
    Gen.oneOf(draftGen, checkoutGen, submittedGen, deliveredGen)
}
