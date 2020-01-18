package com.tst.promos

import com.typesafe.scalalogging.Logger


object PromoService {

  // The PromotionCombo Case Class really isn't useful for us internally. Instead we'll use a Sequence of Promotion
  private type PromoPath = List[Promotion]
  private val logger = Logger("PromoService")

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
    if (allPromotions.exists(p => p.code == promotionCode)) {
      val promotionCombos = getPromotions(allPromotions, Some(promotionCode))
      // The expectation is for the given promotionCode to be at the start, reorder our combos
      promotionCombos.map(combo => {
        combo.copy(promotionCode +: combo.promotionCodes.filterNot(_ == promotionCode))
      })
    } else {
      logger.error("given Promotion Code (%s) does not exist in Promotion List")
      Nil
    }
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

    if (allPromotions.isEmpty) {
      return Nil
    }

    // Some behaviors change if we're limiting combos to those starting with a given promotion code
    val allMode: Boolean = promotionCode.isEmpty

    // if we're doing a promotion code based query this will return the Promotion matching the code
    lazy val rootPromotion = (for {
      code <- promotionCode
      promotion <- allPromotions.find(_.code == code)
    } yield promotion).get

    // Call recursive search function to find all possible combinations
    val rawCombinations: List[PromoPath] = allPromotions.toList.flatMap(p => getCombinations(allPromotions.toList, allMode, p :: Nil))

    // If were not querying all, filter out all matches not containing the given root promotion
    val filteredCombinations = rawCombinations.filter(x => allMode || x.contains(rootPromotion))

    // So far we've been working with collections of Promotion, map to PromotionCombo now.
    val combos: Seq[PromotionCombo] = filteredCombinations.map(x => PromotionCombo(x.map(y => y.code).sorted))

    // If this is not an all search then duplicates can seep in
    val distinctCombos = if (!allMode) {
      combos.distinct
    } else {
      combos
    }

    // Finally, remove any paths which are contained in longer paths (better promo combos)
    distinctCombos.filterNot(combo => {
      combos.filterNot(other => combo == other).exists(other => {
        combo.promotionCodes.diff(other.promotionCodes).isEmpty
      })
    })

  }

  /**
   *
   * Recursive function which transverses a diagonal half of a Cartesian product (in allMode),
   * finding any paths which do not violate exclusions.
   *
   * It's actually more optimized than shown below as exclusions will skip ahead
   *
   * 12345  <- first level always match
   * 2345   <- 1 doesn't need to check itself
   * 345    <- At this point 3 only needs to check 4 and 5 since 1 has checked them all
   * 45
   * 5
   *
   * @param allPromotions all promotions
   * @param allMode       flag indicating type of query
   * @param existing      parent combination
   * @param level         used for keeping track of recursion for debugging, no functional use
   * @return all derivative combos based on the given existing base
   */
  private def getCombinations(allPromotions: PromoPath,
                              allMode: Boolean = false,
                              existing: PromoPath = Nil,
                              level: Int = 0): List[PromoPath] = {

    logger.debug("level %d - %s".format(level, existing.map(x => x.code)))

    val existing_blocks = existing.flatMap(_.notCombinableWith)

    /**
     * Starting with all possible promotions, filter out those we cannot combine with.
     * Additional performance improvement for "allMode"
     */
    val next = allPromotions
      // Remove ones already in the combo
      .diff(existing)
      // Remove Promotions the existing combo blocks
      .filterNot(x => existing_blocks.contains(x.code))
      // Remove Promotions that would themselves conflict with the existing Combo
      .filterNot(x => x.notCombinableWith.intersect(existing.map(_.code)).nonEmpty)
      // The last is a performance improvement for "all" query.
      .filterNot(x => allMode && existing.lastOption.nonEmpty && allPromotions.indexOf(x) < allPromotions.indexOf(existing.last))

    logger.debug("\t next %s".format(next.map(x => x.code)))

    // Recurse further down with new matches
    existing :: next.flatMap(n => {
      getCombinations(allPromotions, allMode, existing ::: n :: Nil, level + 1)
    })
  }

}
