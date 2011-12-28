package starling.gui

import javax.swing.ImageIcon
import javax.imageio.ImageIO
import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._

object StarlingIcons {

  val MarketData = "/icons/16x16_market_data.png"

  lazy val RowGrandTotals = StarlingIcons.icon("/icons/16x16_stock_top_bottom_totals.png")
  lazy val ColumnTotals = StarlingIcons.icon("/icons/16x16_stock_right_totals.png")
  lazy val RowSubTotals = StarlingIcons.icon("/icons/16x16_stock_sub_totals.png")
  lazy val ColumnSubTotals = StarlingIcons.icon("/icons/16x16_stock_col_sub.png")
  lazy val Chart = StarlingIcons.icon("/icons/chart.png")
  lazy val Copy = StarlingIcons.icon("/icons/16x16_copy.png")
  lazy val Rotate = StarlingIcons.icon("/icons/16x16_rotate_cw.png")
  lazy val Lock = StarlingIcons.icon("/icons/16x16_split.png")
  lazy val ExpandColumnsToFit = StarlingIcons.icon("/icons/16x16_expand_columns.png")
  lazy val Calculate = StarlingIcons.icon("/icons/16x16_calculator.png")
  lazy val ValidationError = StarlingIcons.icon("/icons/validation.png")
  lazy val RemoveZeros = StarlingIcons.icon("/icons/16x16_remove_zeros.png")
  lazy val ClearPivot = StarlingIcons.icon("/icons/16x16_clear_pivot.png")
  lazy val DefaultLayout = StarlingIcons.icon("/icons/16x16_contract.png")
  lazy val Save = StarlingIcons.icon("/icons/16x16_save.png")
  lazy val ResetEdits = StarlingIcons.icon("/icons/16x16_undo.png")

  private val iconCache = CacheFactory.getCache("iconCache")
  private val imageCache = CacheFactory.getCache("imageCache")

  def icon(location:String) = iconCache.memoize(location, new ImageIcon(getResource(location, "Icon")))
  def im(location:String) = imageCache.memoize(location, ImageIO.read(getResource(location, "Image")))

  private def getResource(location: String, kind: String) = try {
    getClass.getResource(location) match {
      case null => throw new RuntimeException("No resource found for " + location)
      case r => r
    }
  } catch {
    case e => throw new Exception("Could not load %s: %s" % (kind, location), e)
  }
}