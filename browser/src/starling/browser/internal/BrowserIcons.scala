package starling.browser.internal

import javax.swing.ImageIcon
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.{FileOutputStream, FileInputStream, File}

object BrowserIcons {
  val Refresh = "/icons/view-refresh.png"
  lazy val Blank10 = BrowserIcons.icon("/icons/10x10_blank.png")

  private val iconCache = new SimpleCache[ImageIcon]()
  private val imageCache = new SimpleCache[BufferedImage]()

  def icon(location:String) = iconCache.memoize(location) { new ImageIcon(getResource(location, "Icon")) }
  def im(location:String) = imageCache.memoize(location) { ImageIO.read(getResource(location, "Image")) }

  private def getResource(location: String, kind: String) = try {
    getClass.getResource(location) match {
      case null => throw new RuntimeException("missing resource " + location)
      case r => r
    }
  } catch {
    case e => throw new RuntimeException("Could not load " + kind + " " + location, e)
  }
}

class SimpleCache[T] {
  val cache = new scala.collection.mutable.HashMap[String,T]
  def memoize(key:String)(value:T) = {
    cache.get(key) match {
      case Some(r) => r
      case None => {
        val r = value
        cache(key) = r
        r
      }
    }
  }
}