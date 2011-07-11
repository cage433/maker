package starling.utils.conversions

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.ImplicitConversions
import java.util.concurrent.atomic.AtomicBoolean


class RichRunnableTest extends TestNGSuite with ShouldMatchers {
  
  @Test def shouldNotInvokeActionWhenWrappingWithRunnable {
    val invoked = new AtomicBoolean(false)

    val runnable = ImplicitConversions.runnable(() => {
      println("setting");
      invoked.set(true)}
    )

    invoked.get should be === false

    runnable.run

    invoked.get should be === true
  }

}
