package com.sageserpent.everythingMustGo

import org.scalacheck.{Gen, Prop}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.Random

class CheckoutTests extends FlatSpec with Checkers {
  val itemDatums = Map("Fred" -> ItemData(price = 20), "Frieda" -> ItemData(price = 30.5))

  val checkout: (Iterable[String]) => Double = Checkout.apply(itemDatums)

  "No items" should "result in no billing" in {
    assert(0 === Checkout(Map.empty)(Iterable.empty))
    assert(0 === checkout(Iterable.empty))
  }
  "One item" should "result in a bill that is its price." in {
    val itemGenerator = Gen.oneOf(itemDatums.keys.toSeq)
    check(Prop.forAllNoShrink(itemGenerator)(item => {
      val Some(ItemData(price, _)) = itemDatums.get(item)
      price === checkout(Seq(item))
    }))
  }

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
    assert(2.05 === Checkout.apply(Checkout.productionItemDatums)(List("Apple", "Apple", "Orange", "Apple")))
  }

  it should "also handle melons" in {
    assert(1 === Checkout.apply(Checkout.productionItemDatums)(List("Melon")))
  }

  it should "show the correct discounting on melons" in {
    assert(2.0 === Checkout.apply(Checkout.productionItemDatums)(List.fill(3)("Melon")))
    assert(3.0 === Checkout.apply(Checkout.productionItemDatums)(List.fill(4)("Melon")))
  }

  "Given a bunch of things with a bill, adding a discounted amount of some stuff" should "increase the price by the discounted amount" in {
    val itemDatumsWithDiscounts = Map("Fred" -> ItemData(price = 20, discount = 2 -> 1), "Frieda" -> ItemData(price = 30.5, discount = 3 -> 2))
    val itemDatumsWithoutDiscounts = itemDatumsWithDiscounts map { case (itemName, itemData) => itemName -> itemData.copy(discount = 1 -> 1) }

    val itemGenerator = Gen.oneOf(itemDatumsWithDiscounts.keys.toSeq)
    val nItemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    check(Prop.forAllNoShrink(nItemsGenerator, itemGenerator)((nItems, item) => {
      val checkoutWithDiscounts = Checkout.apply(itemDatumsWithDiscounts) _
      val basicBill = checkoutWithDiscounts(nItems)
      val ItemData(price, (amountEligibleForDiscount, asIfAmount)) = itemDatumsWithDiscounts(item)
      val extraStuff = Seq.fill(amountEligibleForDiscount)(item)
      val checkoutWithoutDiscounts = Checkout.apply(itemDatumsWithoutDiscounts) _
      val priceOfOneItemWithoutDiscount = checkoutWithoutDiscounts(Seq(item))
      val discountedPriceIncrease = asIfAmount * priceOfOneItemWithoutDiscount
      val totalBill = checkoutWithDiscounts(nItems ++ extraStuff)
      totalBill === basicBill + discountedPriceIncrease
    }))
  }

  "Given a bunch of things with a bill, repeatedly adding an amount of some stuff in increments of one lot up to the discounted amount" should "increase the price by the usual amount in all but one case." in {
    val itemDatumsWithDiscounts = Map("Fred" -> ItemData(price = 20, discount = 2 -> 1), "Frieda" -> ItemData(price = 30.5, discount = 3 -> 2))
    val itemDatumsWithoutDiscounts = itemDatumsWithDiscounts map { case (itemName, itemData) => itemName -> itemData.copy(discount = 1 -> 1) }

    val itemGenerator = Gen.oneOf(itemDatumsWithDiscounts.keys.toSeq)
    val nItemsGenerator = Gen.containerOf[Seq, String](itemGenerator)
    check(Prop.forAll(nItemsGenerator, itemGenerator)((nItems, item) => {
      val checkoutWithDiscounts = Checkout.apply(itemDatumsWithDiscounts) _
      val basicBill = checkoutWithDiscounts(nItems)
      val ItemData(price, (amountEligibleForDiscount, _)) = itemDatumsWithDiscounts(item)
      val checkoutWithoutDiscounts = Checkout.apply(itemDatumsWithoutDiscounts) _
      val priceOfOneItemWithoutDiscount = checkoutWithoutDiscounts(Seq(item))
      val billsForProgressivelyIncreasingAmountsOfStuff = 1 to amountEligibleForDiscount map ((amount: Int) => {
        val extraStuff = Seq.fill(amount)(item)
        val totalBill = checkoutWithDiscounts(nItems ++ extraStuff)
        totalBill
      })
      val differencesInTheBills = basicBill +: billsForProgressivelyIncreasingAmountsOfStuff sliding 2 filter (2 == _.size) map ({
        case Seq(first, second) => second - first
      })
      val numberOfDifferencesWithNoDiscountObserved = differencesInTheBills count (priceOfOneItemWithoutDiscount == _)
      amountEligibleForDiscount - 1 === numberOfDifferencesWithNoDiscountObserved
    }))
  }
}
