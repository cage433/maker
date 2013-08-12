package maker

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import maker.utils.ScreenUtils

class MakerRepl extends ILoop{
  override def prompt = "==> "

  addThunk {
    intp.beQuietDuring {
      intp.addImports("java.lang.Math._")
    }
  }

  override def printWelcome() {
    ScreenUtils.clear
    echo("\nWelcome to Maker - enter 'help' for commands")
  }
}

object MakerRepl{
  def exec{
    val settings = new Settings
    settings.usejavacp.value = true
    settings.deprecation.value = true

    new MakerRepl().process(settings)
  }
}
