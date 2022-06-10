package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatus._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.Instant

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val oneItem = List(Item(ItemId("A1"), 2, 12.99))
    val order = Order(
      id = OrderId("AAA"),
      status = Draft(oneItem),
      createdAt = Instant.now()
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout(NEL.fromList(oneItem).get, None))
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = OrderId("AAA"),
      status = Draft(List.empty),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("submit successful example") {
    val oneItem    = NEL.one(Item(ItemId("A1"), 2, 12.99))
    val oneAddress = Address(12, "E16 8TR")
    val order = Order(
      id = OrderId("AAA"),
      status = Checkout(oneItem, Some(oneAddress)),
      createdAt = Instant.now()
    )
    val submittedAt = Instant.now()

    order.submit(submittedAt) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted(oneItem, oneAddress, submittedAt))
    }
  }

  test("submit no address example") {
    val oneItem = NEL.one(Item(ItemId("A1"), 2, 12.99))
    val order = Order(
      id = OrderId("AAA"),
      status = Checkout(oneItem, None),
      createdAt = Instant.now()
    )

    assert(order.submit(Instant.now()) == Left(MissingDeliveryAddress))
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
          order <- order.addItems(items)
          order <- order.checkout
          order <- order.updateDeliveryAddress(deliveryAddress)
          order <- order.submit(submittedAt)
          order <- order.deliver(deliveredAt)
        } yield order

        assert(
          result == Right(
            Order(
              id = orderId,
              status =
                Delivered(NEL.fromList(items.toList).get, deliveryAddress, submittedAt = submittedAt, deliveredAt),
              createdAt = createdAt
            )
          )
        )
    }
  }
}
