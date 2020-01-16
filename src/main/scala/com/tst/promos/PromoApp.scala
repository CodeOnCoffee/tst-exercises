package com.tst.promos

import com.tst.promos.PromoService._

import scala.language.postfixOps

/**
 * Main Class for Promos
 */
object PromoApp extends App {
  val promos = List(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2")),
  )
  args.toList match {
    case "Promos" :: Nil =>
      val all = allCombinablePromotions(promos)
      println("All Combos:")
      println(all)

    case "Promo" :: id :: Nil =>
      val all = combinablePromotions(id, promos)
      println("Combos for %s:".format(id))
      println(all)

  }

}
