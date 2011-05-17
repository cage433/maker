package starling.gui

import javax.swing.ImageIcon
import javax.imageio.ImageIO
import starling.utils.cache.CacheFactory
import starling.utils.ImplicitConversions._

object StarlingIcons {

  val Refresh = "/icons/22x22/actions/view-refresh.png"
  val MarketData = "/icons/16x16_market_data.png"

  lazy val RowGrandTotals = StarlingIcons.icon("/icons/16x16_stock_bottom_totals.png")
  lazy val ColumnTotals = StarlingIcons.icon("/icons/16x16_stock_right_totals.png")
  lazy val RowSubTotals = StarlingIcons.icon("/icons/16x16_stock_sub_totals.png")
  lazy val ColumnSubTotals = StarlingIcons.icon("/icons/16x16_stock_col_sub.png")
  lazy val Chart = StarlingIcons.icon("/icons/chart.png")
  lazy val Copy = StarlingIcons.icon("/icons/16x16_copy.png")
  lazy val Rotate = StarlingIcons.icon("/icons/16x16_rotate_cw.png")
  lazy val Lock = StarlingIcons.icon("/icons/16x16_split.png")
  lazy val Calculate = StarlingIcons.icon("/icons/16x16_calculator.png")
  lazy val SaveLayout = StarlingIcons.icon("/icons/16x16_layout_add.png")
  lazy val ValidationError = StarlingIcons.icon("/icons/validation.png")

  val SaveReportConfiguration = "/icons/16x16_star_add.png"

  private val iconCache = CacheFactory.getCache("iconCache")
  private val imageCache = CacheFactory.getCache("imageCache")

  def icon(location:String) = iconCache.memoize(location, new ImageIcon(getResource(location, "Icon")))
  def im(location:String) = imageCache.memoize(location, ImageIO.read(getResource(location, "Image")))

  private def getResource(location: String, kind: String) = try {
    getClass.getResource(location)
  }
  catch {
    case e => throw new Exception("Could not load %s: %s" % (kind, location), e)
  }
}