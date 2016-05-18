package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemDatums: Map[String, ItemData])(items: Iterable[String]): Double = {
    require(items.forall(itemDatums.contains(_)))
    val itemLots = items.groupBy(identity).mapValues(_.size)
    itemLots map {case (item, lot) => {
      val ItemData(price, (amountEligibleForDiscount, asIfAmount)) = itemDatums(item)
      val discountMultipleCount = lot / amountEligibleForDiscount // Need to truncate, and based on the lots from the customer!
      val effectiveAmount = asIfAmount * discountMultipleCount + lot % amountEligibleForDiscount
      effectiveAmount * price
    }} sum
  }

  val productionItemDatums = Map("Apple" -> ItemData(price = 0.6), "Orange" -> ItemData(price = 0.25), "Melon" -> ItemData(price = 1.0, discount = 3 -> 2))
}
