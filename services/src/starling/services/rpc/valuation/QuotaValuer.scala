package starling.services.rpc.valuation

import com.trafigura.edm.physicaltradespecs.EDMQuota
import starling.quantity.Quantity

class QuotaValuer {
  def getQuota(quotaID : Int) : EDMQuota   = null
  def value(quotaID : Int) : Quantity = value(getQuota(quotaID))
  def value(quota : EDMQuota) : Quantity = {
    val detail = quota.detail
    val pricingSpec = detail.pricingSpec
    val qty = pricingSpec.quantity
    null

  }
}