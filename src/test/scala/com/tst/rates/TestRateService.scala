package com.tst.rates

import com.tst.rates.RateService._
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class TestRateService extends AsyncFlatSpec with MustMatchers {

  "fetching rates" should "succeed" in {
    val futureRates = fetchRates()

    futureRates.map(rates => {
      rates must not be empty
      rates must have size 5
    })
  }

  "fetching cabin prices" should "succeed" in {
    val futurePrices = fetchCabinPrices()

    futurePrices.map(prices => {
      prices must not be empty
      prices must have size 8
    })
  }

  it should "return best prices" in {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("F1", "Farmers"), // ignored
    )
    val cabins = List(
      CabinPrice("CB", "M1", 230.00), //best
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CA", "M1", 200.00), // best
      CabinPrice("CA", "M2", 250.00),
    )
    val bestGroupPrices = getBestGroupPrices(rates, cabins)
    bestGroupPrices must not be empty
    bestGroupPrices must have size 2
    bestGroupPrices must contain(BestGroupPrice("CA", "M1", 200.0, "Military"))
    bestGroupPrices must contain(BestGroupPrice("CB", "M1", 230.0, "Military"))
  }

  it should "not error with non matching requests" in {
    val rates = List(
      Rate("F1", "Farmers"),
      Rate("F2", "Farmers"),
    )
    val cabins = List(
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
    )
    val bestGroupPrices = getBestGroupPrices(rates, cabins)
    bestGroupPrices mustBe empty
  }

  it should "handle empty inputs" in {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
    )
    val cabins = List(
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
    )

    getBestGroupPrices(Nil, cabins) mustBe empty
    getBestGroupPrices(rates, Nil) mustBe empty
    getBestGroupPrices(Nil, Nil) mustBe empty
  }
}
