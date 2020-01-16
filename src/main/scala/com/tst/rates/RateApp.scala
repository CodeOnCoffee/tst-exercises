package com.tst.rates

import com.tst.rates.RateService._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object RateApp extends App {

  // Asynchronously load data
  val data = for {
    rates <- fetchRates()
    cabins <- fetchCabinPrices()
  } yield (rates, cabins)

  // Get Best Prices
  val bestPrices = data.map({
    case (rates, prices) => getBestGroupPrices(rates, prices)
  })

  // At this point we're async but someone has to wait and keep the main Thread open
  Await.result(bestPrices, 5.seconds)

  // Print out success or fail
  bestPrices.onComplete {
    case Success(result) =>
      result.foreach({
        case BestGroupPrice(cabinCode, rateCode, price, rateGroup) =>
          println(s"%s - %s - %s - %s".format(cabinCode, rateCode, price, rateGroup))
      })

    case Failure(e) => throw e
  }

}
