import scala.util.Properties

Properties.setProp("scala.usejavacp", "false")
Properties.setProp("log4j.ignoreTCL", "true")

// All maker settings are compiled. This improves startup time consideably.
import maker.Maker._


def pid = maker.utils.os.ProcessID().id
import topLevel._
import maker.task.FailingTests._
