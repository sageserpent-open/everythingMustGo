package com.sageserpent.everythingMustGo

object Checkout {
  def apply(itemDatums: Map[String, ItemData])(items: Iterable[String]): Double = items map (itemDatums(_).price) sum


  val productionItemDatums = Map("Apple" -> ItemData(price = 0.6), "Orange" -> ItemData(price = 2.05))
}
