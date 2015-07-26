package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemDatums: Map[String, ItemData])(items: Iterable[String]): Double = {
    require(items.forall(itemDatums.contains(_)))
    val itemLots = items.groupBy(identity).mapValues(_.size)
    itemLots map {case (item, lot) => {
      val ItemData(price, (amountEligibleForDiscount, asIfAmount)) = itemDatums(item)
      val effectiveAmount = asIfAmount * lot / amountEligibleForDiscount + lot % amountEligibleForDiscount
      effectiveAmount * price
    }} sum
  }

  val productionItemDatums = Map("Apple" -> ItemData(price = 0.6), "Orange" -> ItemData(price = 2.05))
}
