package com.tst.promos

import com.tst.promos.PromoService._
import org.scalatest.{FlatSpec, MustMatchers}

private class TestPromoService extends FlatSpec with MustMatchers {
  val promos = List(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2")),
  )
  "PromoService" should "Find all Combos" in {
    allCombinablePromotions(promos) must contain only(
      PromotionCombo(List("P1", "P2")),
      PromotionCombo(List("P1", "P4", "P5")),
      PromotionCombo(List("P2", "P3")),
      PromotionCombo(List("P3", "P4", "P5")),
    )
  }

  it should "Find only Combos with P1" in {
    combinablePromotions("P1", promos) must contain only(
      PromotionCombo(List("P1", "P2")),
      PromotionCombo(List("P1", "P4", "P5")),
    )
  }

  it should "Find only Combos with P3" in {
    combinablePromotions("P3", promos) must contain only(
      PromotionCombo(List("P3", "P2")),
      PromotionCombo(List("P3", "P4", "P5")),
    )
  }

  it should "Not care if a given promo code doesn't exist" in {
    combinablePromotions("M1", promos) mustBe empty
  }

  it should "Not care if the list of promos is empty" in {
    allCombinablePromotions(Nil) mustBe empty
    combinablePromotions("M1", Nil) mustBe empty
  }
}
