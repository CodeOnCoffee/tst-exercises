package com.tst.promos

import com.typesafe.scalalogging.Logger


object PromoService {

  private val logger = Logger("PromoService")

  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    getPromotions(allPromotions, Some(promotionCode))
  }

  /**
   *
   * Recursive function which transverses a diagonal half of a Cartesian product,
   * finding any paths which do not violate exclusions
   *
   * It's actually more optimized than shown below as exclusions will skip ahead
   *
   * 12345  <- first level always match
   * 2345   <- 1 doesn't need to check itself
   * 345    <- At this point 3 only needs to check 4 and 5 since 1 has checked them all
   * 45
   * 5
   *
   * Actual Example:
   *
   * existing combo: P2
   *    searching: P3, P4, P5   (Note P1 and P2 were skipped)
   * combo: P2, P3
   *    searching: P4, P5
   *    (P2 excluded P4 and P5)
   * execution ends with the following Combos: (P2), (P2, P3)
   *
   * @param allPromotions all promotions
   * @param promotionCode promotion code to base the search on
   * @param existing      parent combination
   * @param level         used for keeping track of recursion for debugging, no functional use
   * @return all derivative combos based on the given existing base
   */
  private def getCombinations(allPromotions: Seq[Promotion],
                              promotionCode: Option[String],
                              existing: List[Promotion] = Nil,
                              level: Int = 0): List[List[Promotion]] = {

    val position = level
    logger.debug("level %d - %s".format(position, existing.map(x => x.code)))

    val lastOfPath = if (existing.isEmpty) 0 else existing.last
    val next = promotionCode match {
      case Some(_) if existing.nonEmpty =>
        allPromotions.filterNot(x => x.code == existing.last.code)

      case Some(_) if existing.isEmpty =>  // promotionCode query, but not found in list of Promotions
        allPromotions.takeRight(allPromotions.length - allPromotions.indexOf(lastOfPath) - 1)

      case None => allPromotions.takeRight(allPromotions.length - allPromotions.indexOf(lastOfPath) - 1)
    }

    logger.debug("\t trying next %s".format(next.map(x => x.code)))

    val nextMatches: Seq[List[Promotion]] = next.flatMap(n => {
      val existing_blocks = existing.flatMap(_.notCombinableWith)
      if (existing.contains(n) || existing_blocks.contains(n.code) || n.notCombinableWith.intersect(existing.map(e => e.code)).nonEmpty) {
        Nil
      } else {
        getCombinations(allPromotions, promotionCode, existing ::: n :: Nil, level + 1)
      }
    })
    nextMatches.toList ++ (existing :: Nil)
  }


  private def getPromotions(allPromotions: Seq[Promotion], promotionCode: Option[String] = None): Seq[PromotionCombo] = {

    // Start Execution!

    val isPromotionCodeQuery: Boolean = promotionCode match {
      case None => false
      case Some(_) => true
    }

    // We support both full query and limited promo query
    val startingQuery = if (isPromotionCodeQuery) {
      allPromotions.filter(x => x.code == promotionCode.get).toList
    } else {
      Nil
    }

    val rawCombinations: Seq[List[Promotion]] = getCombinations(allPromotions, promotionCode, startingQuery)

    // So far we've been working with collections of Promotion, map to PromotionCombo now
    // At the same time sort the tail of all combos
    val combos: Seq[PromotionCombo] = rawCombinations.map(x => PromotionCombo(x.map(y => y.code) match {
      case head :: tail => head :: tail.sorted
      case x => x
    }))

    // We only care about combinations of 2 or more, our naive algorithm returns single Promo combos
    val combosFiltered: Seq[PromotionCombo] = combos.filter(c => c.promotionCodes.length > 1)

    // If this is a search based on a promotionCode then duplicates can seep in
    val distinctCombos = if (isPromotionCodeQuery) {
      combosFiltered.distinct
    } else {
      combosFiltered
    }

    // Finally, remove any paths which are contained in longer paths (better promo combos)
    distinctCombos.filterNot(combo => {

      // Diff promos with others, if the difference removes all of our promos then the other is greater
      combos.filterNot(other => combo == other).exists(other => {
        combo.promotionCodes.diff(other.promotionCodes).isEmpty
      })

    })
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = getPromotions(allPromotions)

}
