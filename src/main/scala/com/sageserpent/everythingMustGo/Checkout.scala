package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemPrices: Map[String, ItemData], items: Iterable[String]): Double = items map (itemPrices(_).price) sum

  val productionItemMap = Map("Apple" -> ItemData(price = 0.6), "Orange" -> ItemData(price = 2.05))
}
