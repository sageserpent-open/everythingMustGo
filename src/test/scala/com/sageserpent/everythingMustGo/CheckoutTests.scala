package com.sageserpent.everythingMustGo

import org.scalatest.FlatSpec

class CheckoutTests extends FlatSpec {
"No items" should "result in no billing" in {
  0 === Checkout(Iterable.empty)
}
}
