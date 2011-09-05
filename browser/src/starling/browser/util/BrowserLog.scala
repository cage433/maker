package starling.browser.util

object BrowserLog {
  def infoWithTime[T](message:String)(f: =>T) = {
    val oldThreadName = Thread.currentThread.getName
    val start = System.currentTimeMillis()
    try {
      Thread.currentThread.setName(oldThreadName + " > " + message)
      info(message + " Start")
      val result = f;
      println (message + " Complete. Time: " + milliToHumanString(System.currentTimeMillis() - start))
      result
    } finally {
      Thread.currentThread.setName(oldThreadName)
    }
  }

  private def milliToHumanString(milli:Long):String = {
    if (milli < 1000) {
      milli + "(ms)"
    } else if (milli < 60*1000) {
      (milli / 1000) + "(s) " + (milli%1000) + "(ms)"
    } else {
      (milli / (60*1000)) + "(m) " + ((milli/1000)%60) + "(s)"
    }
  }


  def info(message:String) { println(message) }

}