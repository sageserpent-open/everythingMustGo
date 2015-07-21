package com.sageserpent.everythingMustGo

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Prop
import org.scalacheck.Gen

class CheckoutTests extends FlatSpec with Checkers {
  "No items" should "result in no billing" in {
    assert(0 === Checkout(Map.empty, Iterable.empty))
  }
  "One item" should "result in a bill that is its price." in {
    val item = "Alpha"
    val price = 10
    assert(price === Checkout.apply(Map(item -> price), Seq(item)))
  }

  "N+1 items" should "result in the same bill as purchasing the first N and the last one separately and taking the total." in {
    val itemPrices = Map[String, Double]("Fred" -> 20, "Frieda" -> 30.5)
    val itemGenerator = Gen.oneOf(itemPrices.keys.toSeq)
    val nItemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val testCaseGenerator = for {nItems <- nItemsGenerator
                                 item <- itemGenerator} yield nItems -> item
    check(Prop.forAll(testCaseGenerator){case (nItems, item) => {
      val allInOneGoBill = Checkout.apply(itemPrices, nItems :+ item)
      val nItemsBill = Checkout.apply(itemPrices, nItems)
      val itemBill = Checkout.apply(itemPrices, Seq(item))
      allInOneGoBill === nItemsBill + itemBill
    }})
  }
}
