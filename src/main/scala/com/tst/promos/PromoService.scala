package com.tst.promos

import com.typesafe.scalalogging.Logger


object PromoService {

  private val logger = Logger("PromoService")

  // The PromotionCombo Case Class really isn't useful for us internally. Instead we'll use a Sequence of Promotion
  private type PromoPath = List[Promotion]

  /**
   * Required API
   *
   * Return all Promotion combinations with the given promotion code
   *
   * @param promotionCode promotion code to find combinations for
   * @param allPromotions Sequence of all possible promotions
   * @return all possible combinations
   */
  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    getPromotions(allPromotions, Some(promotionCode))
  }

  /**
   * Required API
   *
   * Return all possible combinations of Promotions
   *
   * @param allPromotions Sequence of all possible promotions
   * @return all possible combinations
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    getPromotions(allPromotions)
  }

  /**
   * The public API methods delegate to this unified API in order to maximize code reuse.
   *
   * Combines capabilities of both public API methods. All combinations can be found or those based on
   * a given promotion code
   *
   * @param allPromotions All known Promotions
   * @param promotionCode Optional promotion code to base search on
   * @return All possible combinations
   */
  private def getPromotions(allPromotions: Seq[Promotion], promotionCode: Option[String] = None): Seq[PromotionCombo] = {

    // Some behaviors change if we're limiting combos to those starting with a given promotion code
    val isPromotionCodeQuery: Boolean = promotionCode.nonEmpty

    // Starting point
    val startingQuery: PromoPath = if (isPromotionCodeQuery) {
      val promotion = allPromotions.filter(x => x.code == promotionCode.get).toList
      if(promotion.isEmpty){
        logger.error("Given Promotion Code not found in the system.")
        return Nil
      }
      promotion
    } else {
      // Full query has no start
      Nil
    }

    val rawCombinations: List[PromoPath] = getCombinations(allPromotions.toList, promotionCode, startingQuery)

    // So far we've been working with collections of Promotion, map to PromotionCombo now.
    // At the same time sort the tail of all combos (matches expectations)
    val combos: Seq[PromotionCombo] = rawCombinations.map(x => PromotionCombo(x.map(y => y.code) match {
        // promotionCode searches always have the promotionCode first regardless of the rest of the promo IDs
      case head :: tail if promotionCode.nonEmpty => head :: tail.sorted
      case path => path.sorted
    }))

    // We only care about combinations of 2 or more, our naive algorithm returns single Promo combos
    val combosFiltered: Seq[PromotionCombo] = combos.filter(c => c.promotionCodes.length > 1)

    // If this is a search based on a promotionCode then duplicates can seep in
    val distinctCombos = combosFiltered.distinct

    // Finally, remove any paths which are contained in longer paths (better promo combos)
    distinctCombos.filterNot(combo => {

      // Diff promos with others, if the difference removes all of our promos then the other is greater
      combos.filterNot(other => combo == other).exists(other => {
        combo.promotionCodes.diff(other.promotionCodes).isEmpty
      })

    })
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
  private def getCombinations(allPromotions: PromoPath,
                              promotionCode: Option[String],
                              existing: PromoPath = Nil,
                              level: Int = 0): List[PromoPath] = {

    logger.debug("level %d - %s".format(level, existing.map(x => x.code)))


    val lastOfPath = if (existing.isEmpty) 0 else existing.last

    // The next candidates differ depending on whether or not we're in promotionCode mode
    // "All" Query is better optimized whereas "promotion code"
    val next = promotionCode match {
      case Some(_) if existing.nonEmpty =>
        allPromotions.filterNot(_ == lastOfPath)
      case None => allPromotions.takeRight(allPromotions.length - allPromotions.indexOf(lastOfPath) - 1)
    }



    logger.debug("\t trying next %s".format(next.map(x => x.code)))

    val nextMatches: Seq[PromoPath] = next.flatMap(n => {
      val existing_blocks = existing.flatMap(_.notCombinableWith)
      if (existing.contains(n) || existing_blocks.contains(n.code) || n.notCombinableWith.intersect(existing.map(e => e.code)).nonEmpty) {
        Nil
      } else {
        getCombinations(allPromotions, promotionCode, existing ::: n :: Nil, level + 1)
      }
    })

    val finalMatches = nextMatches.toList match {
      case Nil =>  existing :: Nil // no additional matches found, return existing
      case results => results
    }

    // Consumer only cares about combinations of 2 or more
    finalMatches.filter(_.length > 1)
  }

}
