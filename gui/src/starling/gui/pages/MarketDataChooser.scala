package starling.gui.pages

import java.awt.{Color, Dimension}
import starling.gui.api._
import swing.Swing._
import swing.event.{Event, SelectionChanged, ButtonClicked, MouseClicked}
import swing._
import starling.utils.{STable, SColumn}
import starling.gui.utils.RichReactor
import RichReactor._
import starling.daterange.{ObservationTimeOfDay, ObservationPoint, Day}
import starling.gui.StarlingLocalCache._
import collection.immutable.{Map, TreeSet}
import starling.gui.{TitledDayPicker, StandardUserSettingKeys}
import starling.browser.common.{ButtonClickedEx, NewPageButton, NListView, MigPanel}
import starling.browser.{Modifiers, ComponentState, PageContext}

class MarketDataChooser(maybeDesk:Option[Desk], pageContext:PageContext, snapshotSelection:Option[SnapshotSelection],
                        showValuationDayPicker:Boolean=false) extends MigPanel("insets 0") {
  private val allPricingGroupsForDesk = pageContext.localCache.pricingGroups(maybeDesk)
  private var snapshots: Map[MarketDataSelection, List[SnapshotIDLabel]] = pageContext.localCache.snapshots(maybeDesk)

  override def enabled = super.enabled
  override def enabled_=(b:Boolean) = {
    super.enabled = b
    marketDataSelection.enabled = b
    marketDataDayPicker.enabled = b
    valuationDayPicker.enabled = b
    thetaDayPicker.enabled = b
    snapshotView.enabled = b
    snapshotScrollPane.enabled = b
    viewButton.enabled = b
  }

  private val snapShotSelectionToUse = snapshotSelection.getOrElse({ getDefaultSnapshotSelection })

  private def getDefaultSnapshotSelection = {
    val marketDataSelection = pageContext.getSetting(StandardUserSettingKeys.InitialMarketDataSelection, MarketDataSelection(allPricingGroupsForDesk.headOption))
    val (day, snapshotEntry) = pageContext.localCache.populatedDays(marketDataSelection).filter(_ < Day.today).lastOption match {
      case Some(day) => (day, CurrentEntry)
      case None => (Day.today.previousBusinessDay(pageContext.localCache.ukBusinessCalendar), CurrentEntry)
    }
    SnapshotSelection(marketDataSelection, Some(snapshotEntry), day)
  }

  private def generateSnapshots(observationDay:Day, marketDataSelection:MarketDataSelection) = {
    val snapshotsForDayAndSelection = snapshots.getOrElse(marketDataSelection, List()).filter(_.snapshotDay <= observationDay).sortWith(_ > _)
    CurrentEntry :: ImportAndSnapshotEntry ::snapshotsForDayAndSelection.map(SnapshotVersionEntry(_))
  }

  private val marketDataSelection = new MarketDataSelectionComponent(pageContext, maybeDesk, snapShotSelectionToUse.marketDataSelection)

  private val marketDataDayPicker = new TitledDayPicker {
    tooltip = "Select observation day"
    day = snapShotSelectionToUse.observationDay
  }

  private val valuationDayPicker = new TitledDayPicker(Some("Valuation Day")) {
    tooltip = "Select Valuation Day"
    day = snapShotSelectionToUse.observationDay
  }

  private val thetaDayPicker = new TitledDayPicker(Some("Theta Day")) {
    tooltip = "Select Day to Use for Theta calculations"
    day = snapShotSelectionToUse.observationDay.nextBusinessDay(pageContext.localCache.ukBusinessCalendar)
  }

  def day = marketDataDayPicker.day
  def day_=(day:Day) { marketDataDayPicker.day = day; valuationDayPicker.day = day }

  private val initialSnapshotEntries:Seq[MarketDataVersionEntry] = {
    generateSnapshots(
      snapShotSelectionToUse.observationDay,
      snapShotSelectionToUse.marketDataSelection)
  }

  private val snapshotView = new NListView(initialSnapshotEntries) {
    tooltip = "Select snapshot or select Import and Snapshot to generate and use a new snapshot"
    renderer = ListView.Renderer(_.label)
    selected = snapShotSelectionToUse.entry.get
  }
  private val snapshotScrollPane = new ScrollPane(snapshotView) {
    val label = new Label("30MMM2010 00:00 00 (s0000)")
    minimumSize = new Dimension(label.preferredSize.width + 20, 50)
  }

  private val viewButton = new NewPageButton {
    text = "View Market Data"
    tooltip = "Views the specified market data"
  }

  private val zeroInterestRatesCheckbox = new CheckBox("Zero Interest Rates") {
    tooltip = "Select this to run reports without applying discounting"
  }

  private val zeroVolsCheckbox = new CheckBox("Zero Vols") {
    tooltip = "Select this to run reports with all volatilities set to 0%"
  }

  def invokeWithCurveIdentifier(pageContext:PageContext, action:CurveIdentifierLabel=>Unit) {
    val state = getState
    val snapshotSelection = state.snapshotSelection
    val pricingGroup = snapshotSelection.marketDataSelection
    def invokeAction(version:MarketDataVersion) {
      val marketDataIdentifier = MarketDataIdentifier(snapshotSelection.marketDataSelection, version)
      val envModifiers = TreeSet[EnvironmentModifierLabel]() ++
        (if (zeroInterestRatesCheckbox.selected) Some(EnvironmentModifierLabel.zeroInterestRates) else None).toList ++
        (if (zeroVolsCheckbox.selected) Some(EnvironmentModifierLabel.zeroVols) else None).toList
      val tradeDay = marketDataDayPicker.day //TODO [19 Oct 2010] should be selected by the user
      action(CurveIdentifierLabel(marketDataIdentifier, EnvironmentRuleLabel.COB, tradeDay, valuationDayPicker.dayAndTime,
        thetaDayPicker.dayAndTime, envModifiers))
    }
    snapshotSelection.entry match {
      case None => invokeAction(SpecificMarketDataVersion(0))
      case Some(CurrentEntry) => invokeAction(SpecificMarketDataVersion(pageContext.localCache.latestMarketDataVersion(snapshotSelection.marketDataSelection)))
      case Some(SnapshotVersionEntry(ss)) => invokeAction(SnapshotMarketDataVersion(ss))
      case Some(ImportAndSnapshotEntry) => pageContext.submit(
        ImportAndSnapshotMarketDataRequest(snapshotSelection.marketDataSelection, snapshotSelection.observationDay),
        (snapshot: SnapshotIDLabel)=>{invokeAction(SnapshotMarketDataVersion(snapshot))}, true)
    }
  }

  add(marketDataSelection, "split, spanx, wrap")
  add(marketDataDayPicker, "grow, flowy")
  add(snapshotScrollPane, "grow")
  if (showValuationDayPicker) {
    add(valuationDayPicker, "grow, flowy")
    add(thetaDayPicker, "grow, flowy")
  }
  add(viewButton, "split, ay top, flowy")
  add(zeroInterestRatesCheckbox, "gapbottom 0")
  add(zeroVolsCheckbox, "gapbottom 0")

  onEDT(onEDT(snapshotView.peer.ensureIndexIsVisible(snapshotView.selection.anchorIndex)))

  updatePopulatedObservationDays(marketDataSelection.selection)

  def updatePopulatedObservationDays(selection:MarketDataSelection) {
    marketDataDayPicker.flagged = pageContext.localCache.populatedDays(selection).toSet
  }
  private def updateSnapshotList(selection:MarketDataSelection) {
    val newSnapshots = generateSnapshots(marketDataDayPicker.day, selection)
    setSnapshotList(newSnapshots)
  }
  private def setSnapshotList(newSnapshots:List[MarketDataVersionEntry]) {
    this.suppressing(snapshotView.selection) {
      snapshotView.listData = newSnapshots
      newSnapshots.headOption match {
        case Some(first) => snapshotView.selected = first
        case None =>
      }
    }
    revalidate
  }
  listenTo(pageContext.remotePublisher)
  reactions += {
    case ExcelObservationDay(name, day) => {
      updatePopulatedObservationDays(marketDataSelection.selection)
    }
    case PricingGroupObservationDay(pricingGroup, day) => {
      updatePopulatedObservationDays(marketDataSelection.selection)
    }
    case _: MarketDataSnapshot => {
      this.snapshots = snapshots
      val selected = snapshotView.selected
      updateSnapshotList(marketDataSelection.selection)
      snapshotView.selected = selected match {
        case ImportAndSnapshotEntry => {
          // If you are over import and snapshot then there is a big chance that you were the one who caused this MarketDataSnapshot update.
          // Therefore we will move the the most recent snapshot. However, this is not great as you might not of caused the update but just
          // happen to have the import and snapshot selected.
          val indexToUse = snapshotView.listData.indexOf(selected) + 1
          if (snapshotView.listData.size > indexToUse) {
            snapshotView.listData(indexToUse)
          } else {
            selected
          }
        }
        case _ => selected
      }
    }
  }
  reactions += {
    case MarketDataSelectionChanged(selection) => {
      updatePopulatedObservationDays(selection)
      if (selection.isNull) {
        snapshotView.enabled = false
        setSnapshotList(List())
      } else {
        snapshotView.enabled = true
        updateSnapshotList(selection)
      }
    }
    case SelectionChanged(`marketDataDayPicker`) => {
      // Change the text field and let other people know about this.
      publish(ObservationDayChanged(this, marketDataDayPicker.day))
      updateSnapshotList(marketDataSelection.selection)
      valuationDayPicker.day = marketDataDayPicker.day
    }
    case SelectionChanged(`valuationDayPicker`) => {
      thetaDayPicker.day = valuationDayPicker.day.nextBusinessDay(pageContext.localCache.ukBusinessCalendar)
    }
    case ButtonClickedEx(`viewButton`, e) => publish(ViewRequested(MarketDataChooser.this, Modifiers.modifiers(e.getModifiers)))
    case MouseClicked(`snapshotView`,_,e,2,_) => publish(ViewRequested(MarketDataChooser.this, Modifiers.modifiersEX(e)))
  }
  listenTo(marketDataSelection, marketDataDayPicker, valuationDayPicker, snapshotView.mouse.clicks, viewButton)

  def getState = MarketDataChooserState(
    SnapshotSelection(marketDataSelection.selection, snapshotView.selectedOption, marketDataDayPicker.day),
    valuationDayPicker.day, thetaDayPicker.day,
    zeroVolsCheckbox.selected, zeroInterestRatesCheckbox.selected)

  def setState(state:MarketDataChooserState, useLatestSnapshotEntry:Boolean = true) {
    val snapshotSelection = state.snapshotSelection

    this.suppressing(marketDataSelection, marketDataDayPicker, valuationDayPicker, snapshotView.selection) {
      marketDataSelection.selection = snapshotSelection.marketDataSelection
      marketDataDayPicker.day = snapshotSelection.observationDay
      val newSnapshots = generateSnapshots(marketDataDayPicker.day, marketDataSelection.selection)
      snapshotView.listData = newSnapshots
      if (useLatestSnapshotEntry) {
        val ssToSelection = if (newSnapshots.length > 1) {
          newSnapshots(1)
        } else {
          newSnapshots(0)
        }
        snapshotView.selected = ssToSelection
      } else {
        snapshotView.selected = snapshotSelection.entry.get
      }
      valuationDayPicker.day = state.valuationDay
      thetaDayPicker.day = state.thetaDay

      zeroInterestRatesCheckbox.selected = state.zeroInterestRates
      zeroVolsCheckbox.selected = state.zeroVols
    }
  }

  def toSTable = {
    STable("Valuation Environment", List(SColumn("Market Data"), SColumn("Observation Day"), SColumn("Snapshot")),
      List(List(marketDataSelection.selection.text, marketDataDayPicker.day.toString, snapshotView.selected.label)))
  }
}



case class ViewRequested(source:MarketDataChooser, modifiers:Modifiers) extends Event
case class SnapshotSelection(marketDataSelection:MarketDataSelection, entry:Option[MarketDataVersionEntry], observationDay:Day)

abstract class MarketDataVersionEntry {
  def label:String
}

case class SnapshotVersionEntry(d:SnapshotIDLabel) extends MarketDataVersionEntry {
  override def toString = label
  def label = d.timestamp.toStringSeconds + " (s" + d.id+")"
}

case object ImportAndSnapshotEntry extends MarketDataVersionEntry {
  override def toString = label
  def label = "Import and Snapshot"
}

case object CurrentEntry extends MarketDataVersionEntry {
  override def toString = label
  def label = "Current"
}

case class MarketDataChooserState(
        snapshotSelection:SnapshotSelection, valuationDay:Day, thetaDay:Day,
        zeroVols:Boolean, zeroInterestRates:Boolean) extends ComponentState

case class ObservationDayChanged(source:MarketDataChooser, observationDay:Day) extends Event

