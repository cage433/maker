package maker

import org.scalatest.FreeSpec
import maker.utils.FileUtils._

class ResourceTests extends FreeSpec {
  "test url" in {
    assert(Resource.build("org.scalacheck scalacheck_2.9.2 1.9").relativeURL === "org/scalacheck/scalacheck_2.9.2/1.9/scalacheck_2.9.2-1.9.jar")
    assert(Resource.build("org.scalacheck scalacheck_2.9.2 1.9").basename === "org.scalacheck-scalacheck_2.9.2-1.9.jar")

    val bookmarkDB = Resource("starling.test.resources", "bookmark-test-db", "1.2", "gz")
    assert(bookmarkDB.relativeURL === "starling/test/resources/bookmark-test-db/1.2/bookmark-test-db-1.2.gz")
    assert(bookmarkDB.basename === "starling.test.resources-bookmark-test-db-1.2.gz")
 }

  "test version resolution" in {
    val versions = Map("scala_version" -> "2.9.2", "jetty_version" -> "1.6")
    assert(Resource.build("org.scalacheck scalacheck_{scala_version} 1.9", resourceVersions = versions) === Resource("org.scalacheck", "scalacheck_2.9.2", "1.9"))
    assert(Resource.build("org.scalacheck_{jetty_version}_mike scalacheck_{scala_version}_{jetty_version}_fred 1.9", resourceVersions = versions) === 
      Resource("org.scalacheck_1.6_mike", "scalacheck_2.9.2_1.6_fred", "1.9"))
    try {
      Resource.build("foo {unknown_version}_bar 1.6", resourceVersions = versions)
      fail("Should have thrown exception")
    } catch {
      case _ : RuntimeException => 
    }
  }

  "test string parsing" in {
    assert(Resource("org", "foo", "1.0", "jar") === Resource.build("  org   foo   1.0 "))
    assert(Resource("org", "foo", "1.0", "zip") === Resource.build("  org   foo   1.0 type:zip "))
    assert(Resource("org", "foo", "1.0", "zip", preferredRepository = Some("mike")) === Resource.build("  org   foo   1.0 type:zip resolver:fred", resourceResolvers = Map("fred" -> "mike")))

    List("org foo", "org", "", "org foo 2.0 extra-term", "org foo 1.0 bad-key:34").foreach{
      s => 
        try {
          Resource.build(s)
          fail(s + " should have failed to parse")
        } catch {
          case _ : RuntimeException => 
        }
    }
  }
}  
