package starling.gui.pages

import starling.rmi.StarlingServer
import starling.gui.Page
import starling.daterange.Timestamp
import starling.gui.api.PageLogInfo

object PageLogger {
  def logPageView(page:Page, server:StarlingServer) {
    val time = new Timestamp
    new Thread(new Runnable{def run = {
      server.logPageView(PageLogInfo(page.text, page.shortText, page.toString, time))  
    }}).start
  }
}