package com.tst.rates

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RateService {

  private val fixtureRates = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior"),
    Rate("E2", "Extra-Terrestrial"),
  )

  private val cabinPrices = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00),
  )

  /**
   * Provide a list of Rates in the future
   * @return Future of Rates
   */
  def fetchRates(): Future[List[Rate]] = Future {
    fixtureRates
  }

  /**
   * Provide a list of Cabin Prices in the future
   * @return Future of Rates
   */
  def fetchCabinPrices(): Future[List[CabinPrice]] = Future {
    cabinPrices
  }

  /**
   * Find best prices for Groups
   *
   * @param rates Sequence of Rates to search over
   * @param prices Sequence of Cabin Prices
   * @return Best Group Prices
   */
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    // get distinct cabin codes and rate groups
    val cabinCodes = prices.map(price => price.cabinCode).distinct
    val rateGroups = rates.map(rate => rate.rateGroup).distinct

    // Create cartesian product
    val crossProduct = for {
      cabinCode <- cabinCodes
      rateGroup <- rateGroups
    } yield (cabinCode, rateGroup)

    val bestPrices = crossProduct.flatMap {
      case (cabinCode, rateGroup) =>

        // find all Rate Codes for group
        val rateCodesForGroup = rates.filter(r => r.rateGroup == rateGroup).map(_.rateCode)

        // find cabin with best rate for given RateGroup and CabinCode
        val bestRate = prices.filter(
          price => rateCodesForGroup.contains(price.rateCode)
            && price.cabinCode == cabinCode
        ).minByOption(_.price)

        bestRate.map(rate => BestGroupPrice(cabinCode, rate.rateCode, rate.price, rateGroup))

    }

    bestPrices
  }
}
