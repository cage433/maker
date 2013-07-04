package maker.project

trait TestUtils {
  def sleepToNextSecond = {
    val currTime = System.currentTimeMillis()
    val waitFor = 1000 - currTime % 1000
    Thread.sleep(waitFor + 100)
  }
}
