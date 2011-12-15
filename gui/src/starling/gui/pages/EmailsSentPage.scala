package starling.gui.pages

import starling.pivot.PivotEdits
import starling.gui.{LocalCacheKeys, StarlingIcons}
import starling.browser.{ServerContext, LocalCache}
import starling.services.EmailService
import starling.daterange.Timestamp
import starling.gui.osgi.StarlingBrowserBundle

case class EmailsSentPage(pps: PivotPageState, latestEmail: Timestamp) extends AbstractPivotPage(pps) {
  type SC = EmailService
  def text = "Emails Sent"
  override def icon = StarlingIcons.im("/icons/16x16_mail.png")
  def bundle = StarlingBrowserBundle.BundleName
  def createServerContext(sc:ServerContext) = sc.lookup(classOf[EmailService])
  def selfPage(pivotPageState:PivotPageState, edits:PivotEdits) = copy(pps = pivotPageState)
  def dataRequest(emailService: EmailService) = emailService.emailsSent(latestEmail, pps.pivotFieldParams)
  override def latestPage(localCache:LocalCache) = copy(latestEmail = localCache.localCache(LocalCacheKeys.LatestEmailEvent))
}





