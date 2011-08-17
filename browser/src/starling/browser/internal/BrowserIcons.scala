package starling.browser.internal

import starling.utils.cache.CacheFactory
import javax.swing.ImageIcon
import javax.imageio.ImageIO
import starling.utils.ImplicitConversions._

object BrowserIcons {
  val Refresh = "/icons/22x22/actions/view-refresh.png"
  lazy val Blank10 = BrowserIcons.icon("/icons/10x10_blank.png")

  private val iconCache = CacheFactory.getCache("browserIconCache")
  private val imageCache = CacheFactory.getCache("browserImageCache")

  def icon(location:String) = iconCache.memoize(location, new ImageIcon(getResource(location, "Icon")))
  def im(location:String) = imageCache.memoize(location, ImageIO.read(getResource(location, "Image")))

  private def getResource(location: String, kind: String) = try {
    getClass.getResource(location)
  } catch {
    case e => throw new Exception("Could not load %s: %s" % (kind, location), e)
  }
}