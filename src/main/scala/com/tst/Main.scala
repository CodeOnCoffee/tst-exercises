package com.tst

import com.tst.promos.PromoApp
import com.tst.rates.RateApp

object Main extends App {
  if (args.isEmpty) {
    println("Usage: sbt \"run [COMMAND]\"")
    println("Available Commands:")
    println("\tRates\t-\tFind Best Rates")
    println("\tPromos\t-\tFind all Promo Combinations")
    println("\tPromo XX\t-\tFind all Promo Combinations for the given promo")
  } else {
    println("Main:")
    args(0) match {
      case "Rates" => RateApp.main(args)
      case "Promos" => PromoApp.main(args)
      case "Promo" => PromoApp.main(args)
      case _ => println("Unknown Command %s".format(args))
    }
  }
}
