package starling.gui.osgi

import starling.manager.{BromptonContext, BromptonActivator}
import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.services.EmailService
import starling.browser.service.internal.HeterogeneousMap
import swing.Publisher
import starling.gui.xstream.GuiStarlingXStream
import starling.browser._
import starling.pivot.PivotFieldsState._
import starling.pivot.Field._
import starling.pivot.SomeSelection._
import starling.pivot.NullableDay._
import starling.daterange.Day
import starling.gui.pages.RabbitEventViewerPage._
import starling.gui.pages.RabbitEventViewerPageState._
import starling.browser.PageFactory._
import starling.gui.pages.EmailsSentPage._
import starling.gui.pages.PivotPageState._
import starling.gui.pages.{EmailsSentPage, RabbitEventViewerPageState, RabbitEventViewerPage, PivotPageState}
import starling.gui.api.RabbitEventReceived
import starling.bouncyrmi.BouncyRMIClient
import starling.gui.{StarlingIcons, LocalCacheKeys, StarlingUtilButtons}
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import starling.pivot._

class MetalsGuiBromptonActivator extends BromptonActivator {
  def start(context: BromptonContext) {
    val rabbitEventViewerService = context.awaitService(classOf[RabbitEventViewerService])
    context.registerService(classOf[BrowserBundle], new MetalsBrowserBundle(rabbitEventViewerService))
  }
}

class MetalsBrowserBundle(rabbitEventService:RabbitEventViewerService) extends BrowserBundle {
  def bundleName = "Metals"
  def marshal(obj: AnyRef) = GuiStarlingXStream.write(obj)
  def unmarshal(text: String) = GuiStarlingXStream.read(text).asInstanceOf[AnyRef]
  override def initCache() = {
    val cache = new HeterogeneousMap[LocalCacheKey]
    import LocalCacheKeys._
    cache(LatestRabbitEvent) = rabbitEventService.latestRabbitEvent
    cache
  }
  override def addListeners(cache:HeterogeneousMap[LocalCacheKey], publisher: Publisher) {
    publisher.reactions += {
      case RabbitEventReceived(latestTimestamp) => {
        cache(LocalCacheKeys.LatestRabbitEvent) = latestTimestamp
      }
    }
  }
  override def utilButtons(context:PageContext) = {
    def rabbitEventPage = new PageFactory {
      def create(serverContext:ServerContext) = {
        val latestRabbitEvent = context.localCache.localCache(LocalCacheKeys.LatestRabbitEvent)
        val initialLayout = DefaultPivotState(PivotFieldsState(
          filters = List(Field("Day") -> SomeSelection(Set(NullableDay(Day.today)))),
          rowFields = List(Field("Starling ID"), Field("Source"), Field("Message Time (UTC)"), Field("Subject"))))
        RabbitEventViewerPage(PivotPageState.default(initialLayout), RabbitEventViewerPageState(latestRabbitEvent))
      }
    }

    def emailsSentPage = PageFactory(_ => EmailsSentPage(PivotPageState(), context.localCache.localCache(LocalCacheKeys.LatestEmailEvent)))

    val rabbitEventButton = new PageButton(
      "Rabbit Event Viewer",
      rabbitEventPage,
      StarlingIcons.im("/icons/32x32_event.png"),
      Some( KeyStroke.getKeyStroke(KeyEvent.VK_R, 0) )
    )

    val emailsSentButton = new PageButton(
      "Emails Sent",
      emailsSentPage,
      StarlingIcons.im("/icons/32x32_mail.png"),
      Some( KeyStroke.getKeyStroke(KeyEvent.VK_M, 0) )
    )

    List(rabbitEventButton, emailsSentButton)
  }
}