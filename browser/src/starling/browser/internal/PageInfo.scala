package starling.browser.internal

import starling.browser.{ComponentState, PageComponent, Bookmark, Page}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.util.concurrent.{Callable, Future, Executors}
import java.io.{BufferedOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import scala.ref.SoftReference
import starling.manager.TimeTree
import java.awt.Graphics2D

/**
 * Holds information about the page being displayed and also pages that have been displayed in the past. These are held onto for a long time
 * so don't add anything large to it, and if you do, make it a SoftReference.
 */
class PageInfo(val page: Page, val pageResponse:SoftReference[PageResponse], val bookmark:Bookmark, var pageComponent:Option[PageComponent],
               var pageComponentSoft:SoftReference[PageComponent], var componentState:Option[ComponentState],
               var refreshPage:Option[(Page,Boolean)], val pageTime:Long,
               val timeTree:TimeTree,
               var componentForFocus:Option[SoftReference[java.awt.Component]]=None) {

  private def generateImageFromComponent:Option[BufferedImage] = {
    def genImage(pc:PageComponent) = {
      val i = new BufferedImage(pc.size.width, pc.size.height, BufferedImage.TYPE_INT_RGB)
      val graphics = i.getGraphics.asInstanceOf[Graphics2D]
      pc.paint(graphics)
      graphics.dispose()
      i
    }

    pageComponent match {
      case Some(pc) => Some(genImage(pc))
      case None => pageComponentSoft.get match {
        case Some(pc) => Some(genImage(pc))
        case None => None
      }
    }
  }

  def image:Option[BufferedImage] = {
    if (future == null) {
      generateImageFromComponent
    } else {
      future.get().get match {
        case Some(bytes) => {
          val byteArrayInputStream = new ByteArrayInputStream(bytes)
          val i = ImageIO.read(byteArrayInputStream)
          byteArrayInputStream.close()
          Some(i)
        }
        case None => generateImageFromComponent
      }
    }
  }

  def bundle = page.bundle

  private val backgroundThread = Executors.newSingleThreadExecutor
  private var future:Future[SoftReference[Array[Byte]]] = null

  def image_=(i:BufferedImage) {
    // I don't want the future to hold onto the BufferedImage so we have to put it in an array and null the element later.
    val iA = Array(i)
    future = backgroundThread.submit(new Callable[SoftReference[Array[Byte]]] {
      def call = {
        val out = new ByteArrayOutputStream
        val bOut = new BufferedOutputStream(out)
        ImageIO.write(iA(0), "png", bOut)
        iA(0) = null
        val bytes = out.toByteArray
        bOut.close()
        new SoftReference(bytes)
      }
    })
  }
}
