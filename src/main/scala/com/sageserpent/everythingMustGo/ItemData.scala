package com.sageserpent.everythingMustGo

case class ItemData(val price: Double, val discount: (Int, Int) = 1 -> 1)
{
  require(0 <= price)
}
