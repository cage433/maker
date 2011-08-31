package starling.gui

import api.{SlideAttributes, ReportParameters, SlideParametersLabel}
import pages.ConfigPanel
import swing.{TextField, Alignment, Label, ComboBox, CheckBox}
import starling.gui.utils.RichReactor._
import swing.event.{EditDone, ButtonClicked, SelectionChanged}
import javax.swing.{JComboBox, DefaultComboBoxModel}
import starling.gui.StarlingLocalCache._
import starling.browser.PageContext
import starling.browser.common.GuiUtils._
import starling.browser.common.{RoundedBorder, MigPanel}

class SlideReportConfigPanel(context:PageContext, reportParameters:ReportParameters) extends MigPanel with ConfigPanel {
  def displayName = "Slides"

  val reportOptionsAvailable = context.localCache.reportOptionsAvailable

  class SlideOptionsPanel(text:String, origSlideParams:Option[SlideParametersLabel])
          extends MigPanel("gapy 4lp", "[" + StandardLeftIndent + "][p][fill][p][p][p]") {
    border = RoundedBorder(colour = PivotTableBackgroundColour)
    val numFieldCols = 4
    val slideParametersAvailable = reportOptionsAvailable.slideParameters
    val slideComboBox = new ComboBox(slideParametersAvailable.map(_.slideType))

    val marketComboBoxModel = new DefaultComboBoxModel
    val marketOrCommodityComboBox = new ComboBox(List("")) {
      override lazy val peer: JComboBox = new JComboBox(marketComboBoxModel) with SuperMixin {
        var layingOut = false
        override def getSize = {
          val s = super.getSize()
          if (!layingOut && getPreferredSize.width != 0) {
            s.width = getPreferredSize.width
          }
          s
        }

        override def doLayout = {
          try {
            layingOut = true
            super.doLayout
          } finally {
            layingOut = false
          }
        }
      }
    }
    val actualType = new Label("Parallel") {
      horizontalAlignment = Alignment.Left
    }

    class UpdatingTextField extends TextField(numFieldCols) {
      reactions += {
        case EditDone(_) => updateRunButton
      }
      listenTo(this)
      minimumSize = preferredSize
    }
    def verifyDouble(s:String) = try {s.toDouble;true} catch {case t:Throwable => false}
    def verifyInt(s:String) = try {s.toInt;true} catch {case t:Throwable => false}

    val stepSizeField = new UpdatingTextField
    stepSizeField.shouldYieldFocus = verifyDouble
    stepSizeField.verifier = verifyDouble
    val stepUOMLabel = new Label("")
    val upStepsField = new UpdatingTextField
    upStepsField.shouldYieldFocus = verifyInt
    upStepsField.verifier = verifyInt
    val downStepsField = new UpdatingTextField
    downStepsField.shouldYieldFocus = verifyInt
    downStepsField.verifier = verifyInt

    def marketsAvailable(slideType: String): Option[scala.List[SlideAttributes]] = {
      var marketsAvailable: Option[scala.List[SlideAttributes]] = None

      this.suppressing(marketOrCommodityComboBox.selection) {
        val slideParametersSelected = slideParametersAvailable.find(_.slideType == slideType).get

        marketLabel.text = slideParametersSelected.slideTypeType
        upStepsField.text = slideParametersSelected.upSteps
        downStepsField.text = slideParametersSelected.downSteps
        // This is a bit of a hack as I'm just using the text here.
        downStepsField.enabled = (slideType != "Time")
        marketsAvailable = slideParametersSelected.markets
        marketsAvailable match {
          case None => {
            slideParametersSelected.commodities match {
              case None => {
                marketComboBoxModel.removeAllElements
                stepSizeField.text = slideParametersSelected.defaultStepSize.getOrElse("")
                stepUOMLabel.text = slideParametersSelected.defaultUOM.getOrElse("")
                marketComboBoxModel.addElement(" ")
                marketOrCommodityComboBox.enabled = false
              }
              case Some(commodities) => {
                marketComboBoxModel.removeAllElements
                commodities.foreach(com => marketComboBoxModel.addElement(com.marketOrCommodity))
                stepSizeField.text = slideParametersSelected.defaultStepSize.getOrElse("")
                stepUOMLabel.text = slideParametersSelected.defaultUOM.getOrElse("")
                marketOrCommodityComboBox.enabled = true
              }
            }
            marketsAvailable = slideParametersSelected.commodities
          }
          case Some(markets) => {
            marketComboBoxModel.removeAllElements
            markets.foreach(mar => marketComboBoxModel.addElement(mar.marketOrCommodity))
            stepSizeField.text = slideParametersSelected.defaultStepSize.getOrElse("")
            stepUOMLabel.text = slideParametersSelected.defaultUOM.getOrElse("")
            marketOrCommodityComboBox.enabled = true
          }
        }
      }
      marketsAvailable
    }

    def slideTypeChanged(slideType:String) {
      // Update the market combo box and step details.
      marketsAvailable(slideType) match {
        case Some(m) => marketOrCommodityComboBox.selection.index = 0
        case None =>
      }
    }

    val enabledCheckBox = new CheckBox(text)

    add(enabledCheckBox, "spanx, wrap")

    val slideLabel = new Label("Slide:")
    add(slideLabel, "skip 1")
    add(slideComboBox, "wmin 100lp, sgy")

    val stepSizeLabel = new Label("Step Size:")
    add(stepSizeLabel)
    add(stepSizeField)
    add(stepUOMLabel, "wrap")

    val marketLabel = new Label("")
    add(marketLabel, "skip 1")
    add(marketOrCommodityComboBox, "wmin 100lp, sgy")

    val upStepsLabel = new Label("Up Steps:")
    add(upStepsLabel)
    add(upStepsField, "wrap")

    val typeLabel = new Label("Type:")
    add(typeLabel, "skip 1")
    add(actualType, "sgy")

    val downStepsLabel = new Label("Down Steps:")
    add(downStepsLabel)
    add(downStepsField)

    reactions += {
      case SelectionChanged(`slideComboBox`) => {slideTypeChanged(slideComboBox.selection.item); updateRunButton}
      case SelectionChanged(`marketOrCommodityComboBox`) => {
        val slideType = slideComboBox.selection.item
        val market = marketOrCommodityComboBox.selection.item
        // There must be a market or commodity.
        val slideAttributes = slideParametersAvailable.filter(_.slideType == slideType).flatMap(sp => {
          sp.markets match {
            case None => sp.commodities.get
            case Some(m) => m
            }
        }).filter(_.marketOrCommodity == market)(0)
        stepSizeField.text = slideAttributes.stepSize
        slideAttributes.uom match {
          case Some(unit) => stepUOMLabel.text = unit
          case _ =>
        }
        updateRunButton
      }
    }
    listenTo(slideComboBox.selection)
    slideTypeChanged(slideParametersAvailable(0).slideType)

    def slideParameters = {
      if (enabledCheckBox.selected) {
        val (market,commodity) = if (marketOrCommodityComboBox.enabled) {
          val slideType = slideComboBox.selection.item
          val slideParametersSelected = slideParametersAvailable.find(_.slideType == slideType).get
          slideParametersSelected.markets match {
            case None => {
              slideParametersSelected.commodities match {
                case None => (None,None)
                case Some(com) => (None, Some(marketOrCommodityComboBox.selection.item))
              }
            }
            case Some(mar) => (Some(marketOrCommodityComboBox.selection.item), None)
          }
        } else {
          (None,None)
        }
        Some(SlideParametersLabel(slideComboBox.selection.item, market, stepSizeField.text.trim.toDouble,
          upStepsField.text.trim.toInt, downStepsField.text.trim.toInt, stepUOMLabel.text, commodity, marketLabel.text))
      } else {
        None
      }
    }

    def childrenEnabled_=(b:Boolean) {
      slideComboBox.enabled = b
      stepSizeField.enabled = b
      upStepsField.enabled = b
      downStepsField.enabled = b
      stepUOMLabel.enabled = b
      marketOrCommodityComboBox.enabled = b
      slideLabel.enabled = b
      stepSizeLabel.enabled = b
      marketLabel.enabled = b
      upStepsLabel.enabled = b
      typeLabel.enabled = b
      downStepsLabel.enabled = b
      actualType.enabled = b
    }
    def childrenEnabled = true // not used.

    reset(origSlideParams)

    def reset(sp0:Option[SlideParametersLabel]) {
      sp0 match {
        case None => {
          enabledCheckBox.selected = false
          childrenEnabled = false
        }
        case Some(sp) => {
          enabledCheckBox.selected = true
          childrenEnabled = true
          slideComboBox.selection.item = sp.slideType
          sp.market.map(m => marketOrCommodityComboBox.selection.item = m)
          sp.commodity.map(c => marketOrCommodityComboBox.selection.item = c)
          stepSizeField.text = sp.stepSize.toString
          upStepsField.text = sp.upSteps.toString
          marketLabel.text = sp.slideTypeType
          downStepsField.text = sp.downSteps.toString
          stepUOMLabel.text = sp.uom
        }
      }
    }
  }

  private def updateRunButton = publish(UpdateRunButtonEvent(this))

  def slideConfig = SlideConfig(slide1Panel.slideParameters,slide2Panel.slideParameters)

  val slide1Panel = new SlideOptionsPanel("1D Slide", reportParameters.reportOptions.slide1)
  val slide2Panel = new SlideOptionsPanel("2D Slide", reportParameters.reportOptions.slide2)

  override def revert() {setupState(reportParameters)}

  def setupState(rp:ReportParameters) {
    slide1Panel.reset(rp.reportOptions.slide1)
    slide2Panel.reset(rp.reportOptions.slide2)
  }

  reactions += {
    case ButtonClicked(slide1Panel.enabledCheckBox) => {
      val en = slide1Panel.enabledCheckBox.selected
      slide1Panel.childrenEnabled = en
      slide2Panel.enabledCheckBox.enabled = en
      if (!en) {
        slide2Panel.enabledCheckBox.selected = false
        slide2Panel.childrenEnabled = false
      }
      updateRunButton
    }
    case ButtonClicked(slide2Panel.enabledCheckBox) => {
      slide2Panel.childrenEnabled = slide2Panel.enabledCheckBox.selected
      updateRunButton
    }
  }
  listenTo(slide1Panel.enabledCheckBox, slide2Panel.enabledCheckBox)

  slide2Panel.enabledCheckBox.enabled = slide1Panel.enabledCheckBox.selected

  add(slide1Panel)
  add(slide2Panel)
}

case class SlideConfig(slide1:Option[SlideParametersLabel], slide2:Option[SlideParametersLabel])
object SlideConfig {
  val NoSlides = SlideConfig(None, None)
}