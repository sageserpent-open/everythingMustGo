package com.sageserpent.everythingMustGo

import org.scalatest.FlatSpec

class CheckoutTests extends FlatSpec {
"No items" should "result in no billing" in {
  assert(0 === Checkout(Map.empty, Iterable.empty))
}
  "One item" should "result in a bill that is its price." in {
    val item = "Alpha"
    val price = 10
    assert(price === Checkout.apply(Map(item -> price), Seq(item)))
  }
}
