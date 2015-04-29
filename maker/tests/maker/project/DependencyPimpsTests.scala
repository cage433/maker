package maker.project

import org.scalatest.{Matchers, FreeSpec}
import org.eclipse.aether.util.artifact.JavaScopes

class DependencyPimpsTests extends FreeSpec with Matchers with DependencyPimps{
  "toLongString should format as expected" in {
    val dep = "foo" % "bar" % "1.2.3"
    dep.toLongString should equal(""""foo" % "bar" % "1.2.3"""")

    val depWithScope = "foo" % "bar" % "1.2.3" withScope(JavaScopes.TEST)
    depWithScope.toLongString should equal(""""foo" % "bar" % "1.2.3" withScope("test")""")

    val depWithScalaVersion = "foo" % "bar" %% "1.2.3"
    depWithScalaVersion.toLongString should equal(""""foo" % "bar" %% "1.2.3"""")

  }
  
}
