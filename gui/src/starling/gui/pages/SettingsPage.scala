package starling.gui.pages

import java.awt.Dimension
import starling.gui._
import swing.event.ButtonClicked
import starling.browser.common.GuiUtils._
import StandardUserSettingKeys.{ExtraFormattingInfo}
import javax.swing.{SpinnerNumberModel, JSpinner}
import swing._
import starling.daterange.{Day, Month}
import starling.pivot._
import starling.pivot.MonthFormat._
import starling.pivot.utils.PeriodPivotFormatter
import javax.swing.event.{ChangeEvent, ChangeListener}
import starling.browser.{PageComponent, Bookmark, PageData, PageContext}
import starling.browser.common.MigPanel

