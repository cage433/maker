import scala.util.Properties

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")

// All maker settings are compiled. This improves startup time consideably.
import maker.Maker._


def pid = maker.utils.os.ProcessID().id
import maker_._
import maker.task.FailingTests._
import maker.task.compile._
