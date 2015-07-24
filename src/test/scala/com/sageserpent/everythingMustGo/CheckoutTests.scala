package com.sageserpent.everythingMustGo

import org.scalacheck.{Gen, Prop}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.Random

class CheckoutTests extends FlatSpec with Checkers {
  "No items" should "result in no billing" in {
    assert(0 === Checkout(Map.empty)(Iterable.empty))
  }
  "One item" should "result in a bill that is its price." in {
    val item = "Alpha"
    val price = 10
    assert(price === Checkout.apply(Map(item -> ItemData(price = price)))(Seq(item)))
  }

  val itemDatums = Map("Fred" -> ItemData(price = 20), "Frieda" -> ItemData(price = 30.5))

  val checkout: (Iterable[String]) => Double = Checkout.apply(itemDatums)

  "N+1 items" should "result in the same bill as purchasing the first N and the last one separately and taking the total." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    val nItemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val testCaseGenerator = for {nItems <- nItemsGenerator
                                 item <- itemGenerator} yield nItems -> item
    check(Prop.forAll(testCaseGenerator){case (nItems, item) => {
      val allInOneGoBill = checkout(nItems :+ item)
      val nItemsBill = checkout(nItems)
      val itemBill = checkout(Seq(item))
      allInOneGoBill === nItemsBill + itemBill
    }})
  }

  "The order of the items" should "not make any difference to the bill." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    val itemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val seedGenerator = Gen.oneOf(1, 2)
    val testCaseGenerator = for {items <- itemsGenerator
                                 seed <- seedGenerator
                                 permutation = new Random(seed).shuffle(_: Seq[String])} yield items -> permutation
    check(Prop.forAll(testCaseGenerator){case (items, permutation) => {
      val billForItemsOneWay = checkout(items)
      val billForItemsAnotherWay = checkout(permutation(items))
      billForItemsAnotherWay === billForItemsOneWay
    } })
  }

  "A bill" should "not be negative." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    val testCaseGenerator = Gen.containerOf[Seq, String](itemGenerator)
    check(Prop.forAll(testCaseGenerator)(items => 0 <= checkout(items)))
  }

  "An acceptance test" should "be honoured in the observance and not the breach" in {
    2.05 === Checkout.apply(Checkout.productionItemDatums) (List("Apple", "Apple", "Orange", "Orange"))
  }


}
