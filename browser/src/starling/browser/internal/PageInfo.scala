package starling.browser.internal

import starling.browser.{ComponentState, PageComponent, Bookmark, Page}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.util.concurrent.{Callable, Future, Executors, CountDownLatch}
import java.io.{BufferedOutputStream, ByteArrayOutputStream, ByteArrayInputStream}
import scala.ref.SoftReference

class PageInfo(val page: Page, val pageResponse:PageResponse, val bookmark:Bookmark, var pageComponent:Option[PageComponent],
               var pageComponentSoft:SoftReference[PageComponent], var componentState:Option[ComponentState],
               var refreshPage:Option[Page], var componentForFocus:Option[java.awt.Component]=None) {
  def image:BufferedImage = {
    if (future == null) {
      null
    } else {
      val byteArrayInputStream = new ByteArrayInputStream(future.get)
      val i = ImageIO.read(byteArrayInputStream)
      byteArrayInputStream.close
      i
    }
  }

  def bundle = page.bundle

  private val backgroundThread = Executors.newSingleThreadExecutor
  private var future:Future[Array[Byte]] = null

  def image_=(i:BufferedImage) {
    // I don't want the future to hold onto the BufferedImage so we have to put it in an array and null the element later.
    val iA = Array(i)
    future = backgroundThread.submit(new Callable[Array[Byte]] {
      def call = {
        val out = new ByteArrayOutputStream
        val bOut = new BufferedOutputStream(out)
        ImageIO.write(iA(0), "png", bOut)
        iA(0) = null
        val bytes = out.toByteArray
        bOut.close
        bytes
      }
    })
  }
}
