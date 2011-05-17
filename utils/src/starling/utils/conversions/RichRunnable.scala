package starling.utils.conversions


trait RichRunnable {
  implicit def runnable(f: () => Unit): Runnable = new Runnable() { def run() { f() } }
}
