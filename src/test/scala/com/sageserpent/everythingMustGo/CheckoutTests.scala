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
    check(Prop.forAllNoShrink(nItemsGenerator, itemGenerator)((nItems, item) => {
      val allInOneGoBill = checkout(nItems :+ item)
      val nItemsBill = checkout(nItems)
      val itemBill = checkout(Seq(item))
      allInOneGoBill === nItemsBill + itemBill
    }))
  }

  "The order of the items" should "not make any difference to the bill." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    val itemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val seedGenerator = Gen.oneOf(1, 2)
    val permutationGenerator = for {seed <- seedGenerator} yield new Random(seed).shuffle(_: Seq[String])
    check(Prop.forAllNoShrink(itemsGenerator, permutationGenerator)((items, permutation) => {
      val billForItemsOneWay = checkout(items)
      val billForItemsAnotherWay = checkout(permutation(items))
      billForItemsAnotherWay === billForItemsOneWay
    }))
  }

  "A bill" should "not be negative." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    val itemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    check(Prop.forAllNoShrink(itemsGenerator)(items => 0 <= checkout(items)))
  }

  "An acceptance test" should "be honoured in the observance and not the breach" in {
    2.05 === Checkout.apply(Checkout.productionItemDatums) (List("Apple", "Apple", "Orange", "Orange"))
  }

  "Given a bunch of things with a bill, adding a discounted amount of some stuff" should "increase the price by the discount" in {
    val itemDatumsWithDiscounts = Map("Fred" -> ItemData(price = 20, discount = 2 -> 1), "Frieda" -> ItemData(price = 30.5, discount = 3 -> 2))
    val itemDatumsWithoutDiscounts = itemDatumsWithDiscounts map {case (itemName, itemData) => itemName -> itemData.copy(discount = 1 -> 1)}

    val itemGenerator = Gen.oneOf(itemDatumsWithDiscounts.keys.toSeq)
    val nItemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    val testCaseGenerator = for {nItems <- nItemsGenerator
                                 item <- itemGenerator} yield nItems -> item
    check(Prop.forAll(nItemsGenerator, itemGenerator)((nItems, item) => {
      val checkoutWithDiscounts = Checkout.apply(itemDatumsWithDiscounts) _
      val basicBill = checkoutWithDiscounts (nItems)
      val ItemData(price, (amountEligibleForDiscount, asIfAmount)) = itemDatumsWithDiscounts(item) // Where are the named pattern variables, like in F#? Sigh.
      val extraStuff = Seq.fill(amountEligibleForDiscount)(item)
      val checkoutWithoutDiscounts = Checkout.apply(itemDatumsWithoutDiscounts) _
      val expectedDiscountedPriceIncrease = asIfAmount * checkoutWithoutDiscounts (Seq(item))
      val totalBill = checkoutWithDiscounts (nItems ++ extraStuff)
      totalBill === basicBill + expectedDiscountedPriceIncrease
    }))
}
}
