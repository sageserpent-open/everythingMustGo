package com.sageserpent.everythingMustGo

import org.scalacheck.{Gen, Prop}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.Random

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

  "The order of the items" should "not make any difference to the bill." in {
    val itemPrices = Map[String, Double]("Fred" -> 20, "Frieda" -> 30.5)
    val itemGenerator = Gen.oneOf(itemPrices.keys.toSeq)
    val itemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val seedGenerator = Gen.oneOf(1, 2)
    val testCaseGenerator = for {items <- itemsGenerator
                                 seed <- seedGenerator
                                 permutation = new Random(seed).shuffle(_: Seq[String])} yield items -> permutation
    check(Prop.forAll(testCaseGenerator){case (items, permutation) => {
      val billForItemsOneWay = Checkout.apply(itemPrices, items)
      val billForItemsAnotherWay = Checkout.apply(itemPrices, permutation(items))
      billForItemsAnotherWay === billForItemsOneWay
    } })
  }

  "A bill" should "not be negative." in {
    val itemPrices = Map[String, Double]("Fred" -> 20, "Frieda" -> 30.5)
    val itemGenerator = Gen.oneOf(itemPrices.keys.toSeq)
    val testCaseGenerator = Gen.containerOf[Seq, String](itemGenerator)
    check(Prop.forAll(testCaseGenerator)(items => 0 <= Checkout.apply(itemPrices, items)))
  }

  "An acceptance test" should "be honoured in the observance and not the breach" in {
    2.05 === Checkout.apply(Checkout.productionItemMap, List("Apple", "Apple", "Orange", "Orange"))
  }
}
