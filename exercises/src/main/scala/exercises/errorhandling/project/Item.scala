package exercises.errorhandling.project

case class ItemId(value: String)
case class Item(id: ItemId, quantity: Int, unitPrice: BigDecimal)
