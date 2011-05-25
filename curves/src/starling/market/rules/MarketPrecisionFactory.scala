package starling.market.rules

object MarketPrecisionFactory {
  private var rulesImpl: Option[PrecisionRules] = None

  def registerRulesImpl(rules: PrecisionRules) {
    rulesImpl match {
      case None => rulesImpl = Some(rules)
      case Some(_) => throw new Exception("Implementation already registered")
    }
  }

  /**
   * Same as registerRulesImpl but allows new rules to be registerd
   */
  def registerNewRulesImplForTesting(rules: Option[PrecisionRules]) {
    rulesImpl = rules
  }

  def ruleOpton = rulesImpl

  def rules = {
    rulesImpl match {
    // If you hit this from a test try extending the test class from TestExpiryRules
      case None => throw new Exception("Implementation not yet registered")
      case Some(rules) => rules
    }
  }
}