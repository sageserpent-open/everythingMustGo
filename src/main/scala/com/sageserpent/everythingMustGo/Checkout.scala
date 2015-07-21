package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemPrices: Map[String, Double], items: Iterable[String]): Double = items map (itemPrices(_)) sum

  val productionItemMap = Map("Apple" -> 0.6, "Orange" -> 2.05)
}
