package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatus._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.Instant

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Checkout")
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = Nil,
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(InvalidStatus("Delivered")))
  }

  test("submit successful example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.submit(Instant.now()) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Submitted")
    }
  }

  test("submit no address example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(MissingDeliveryAddress))
  }

  test("submit invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus("Delivered")))
  }

  test("submit empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = Nil,
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(EmptyBasket))
  }

  test("checkout is not allowed if order is in checkout, submitted, or delivered status") {
    forAll(Gen.oneOf(checkoutGen, submittedGen, deliveredGen)) { order =>
      assert(order.checkout == Left(InvalidStatus(order.status)))
    }
  }

  test("happy path") {
    forAll(orderIdGen, instantGen, durationGen, durationGen, nelOf(itemGen), addressGen) {
      (orderId, createdAt, submittedDelay, deliveredDelay, items, deliveryAddress) =>
        val submittedAt = createdAt.plus(submittedDelay)
        val deliveredAt = submittedAt.plus(deliveredDelay)
        val order       = Order.empty(orderId, createdAt)

        val result = for {
          order         <- order.addItems(items)
          order         <- order.checkout
          order         <- order.updateDeliveryAddress(deliveryAddress)
          order         <- order.submit(submittedAt)
          orderDuration <- order.deliver(deliveredAt)
        } yield orderDuration

        assert(
          result.map(_._1) == Right(
            Order(
              id = orderId,
              status = Delivered,
              basket = items.toList,
              deliveryAddress = Some(deliveryAddress),
              createdAt = createdAt,
              submittedAt = Some(submittedAt),
              deliveredAt = Some(deliveredAt)
            )
          )
        )

        assert(result.map(_._2) == Right(deliveredDelay))
    }
  }
}
