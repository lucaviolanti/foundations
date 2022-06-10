package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus._

import java.time.Instant

case class OrderId(value: String)

case class Order(
  id: OrderId,
  status: OrderStatus,
  createdAt: Instant // set when the order is created ("Draft")
) {

  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): Either[OrderError, Order] =
    addItems(NEL.one(item))

  def addItems(items: NEL[Item]): Either[OrderError, Order] =
    status match {
      case o: Draft    => Right(copy(status = Draft(o.basket ++ items.toList)))
      case o: Checkout => Right(copy(status = Draft(o.basket.toList ++ items.toList)))
      case _           => Left(InvalidStatus(status))
    }

  // 1. Implement `checkout` which attempts to move the `Order` to "Checkout" status.
  // `checkout` requires the order to be in the "Draft" status, otherwise it returns an `InvalidStatus` error.
  // `checkout` requires the order to contain at least one item, otherwise it returns an `EmptyBasket` error.
  def checkout: Either[OrderError, Order] =
    status match {
      case o: Draft =>
        NEL.fromList(o.basket) match {
          case None        => Left(EmptyBasket)
          case Some(items) => Right(this.copy(status = Checkout(items, None)))
        }
      case _ => Left(InvalidStatus(status))
    }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case o: Checkout =>
        Right(copy(status = o.copy(deliveryAddress = Some(address))))
      case _ => Left(InvalidStatus(status))
    }

  // 2. Implement `submit` which attempts to move the `Order` to "Submitted" status.
  // `submit` requires the order to be in the "Checkout" status and to have a delivery address.
  // If `submit` succeeds, the resulting order must be in "Submitted" status and
  // have the field `submittedAt` defined.
  // Note: You may need to extend `OrderError`
  def submit(now: Instant): Either[OrderError, Order] =
    status match {
      case o: Checkout =>
        o.deliveryAddress match {
          case None          => Left(MissingDeliveryAddress)
          case Some(address) => Right(copy(status = Submitted(o.basket, address, submittedAt = now)))
        }
      case _ => Left(InvalidStatus(status))
    }

  // 3. Implement `deliver` which attempts to move the `Order` to "Delivered" status.
  // `deliver` requires the order to be in the "Submitted" status.
  // If `deliver` succeeds, the resulting order must be in "Delivered" status and
  // have the field `deliveredAt` defined.
  // If `deliver` succeeds, it also returns the time it took to deliver the order (duration
  // between `submittedAt` and `deliveredAt`).
  // Note: You may need to extend `OrderError`
  def deliver(now: Instant): Either[OrderError, Order] =
    status match {
      case o: Submitted =>
        val newOrder =
          copy(status = Delivered(o.basket, o.deliveryAddress, submittedAt = o.submittedAt, deliveredAt = now))
        Right(newOrder)
      case _ => Left(InvalidStatus(status))
    }
}

object Order {
  // Creates an empty draft order.
  def empty(id: OrderId, now: Instant): Order =
    Order(
      id = id,
      status = Draft(List.empty),
      createdAt = now
    )
}
