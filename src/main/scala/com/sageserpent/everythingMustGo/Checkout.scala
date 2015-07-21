package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemPrices: Map[String, Double], items: Iterable[String]): Double = (for ((_, price) <- itemPrices.headOption) yield price) getOrElse 0
}
