package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemPrices: Map[String, Double], items: Iterable[String]): Double = items map (itemPrices(_)) sum
}
