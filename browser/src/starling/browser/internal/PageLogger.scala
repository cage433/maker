package starling.browser.internal

import starling.daterange.Timestamp
import starling.browser.Page
import starling.browser.service.{PageLogInfo, BrowserService}

object PageLogger {
  def logPageView(page:Page, service:BrowserService) {
    new Thread(new Runnable{def run = {
      service.logPageView(PageLogInfo(page.text, page.shortText, page.toString, new java.util.Date()))
    }}).start
  }
}