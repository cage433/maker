package maker

import org.scalatest.FreeSpec

class ResourceTests extends FreeSpec {
  "test url" in {
    assert(Resource.parse("org.scalacheck scalacheck_2.9.2 1.9").relativeURL === "org/scalacheck/scalacheck_2.9.2/1.9/scalacheck_2.9.2-1.9.jar")
    assert(Resource.parse("org.scalacheck scalacheck_2.9.2 1.9").basename === "org.scalacheck-scalacheck_2.9.2-1.9.jar")

    val bookmarkDB = Resource("starling.test.resources", "bookmark-test-db", "1.2", extension = "gz")
    assert(bookmarkDB.relativeURL === "starling/test/resources/bookmark-test-db/1.2/bookmark-test-db-1.2.gz")
    assert(bookmarkDB.basename === "starling.test.resources-bookmark-test-db-1.2.gz")
 }

  "test version resolution" in {
    val versions = Map("scala_version" -> "2.9.2", "jetty_version" -> "1.6", "scala_version_base" -> "2.9", "scalatest_version" -> "1.23")
    assert(Resource.parse("org.scalacheck scalacheck_{scala_version} 1.9", versions) === Resource("org.scalacheck", "scalacheck_2.9.2", "1.9"))
    assert(Resource.parse("org.scalatest scalatest_{scala_version_base} {scalatest_version}", versions) === Resource("org.scalatest", "scalatest_2.9", "1.23"))
    assert(Resource.parse("org.scalacheck_{jetty_version}_mike scalacheck_{scala_version}_{jetty_version}_fred 1.9", versions) === 
      Resource("org.scalacheck_1.6_mike", "scalacheck_2.9.2_1.6_fred", "1.9"))
    try {
      Resource.parse("foo {unknown_version}_bar 1.6", versions)
      fail("Should have thrown exception")
    } catch {
      case _ : RuntimeException => 
    }
  }

  "test string parsing" in {
    assert(Resource("org", "foo", "1.0", extension = "jar") === Resource.parse("  org   foo   1.0 "))
    assert(Resource("org", "foo", "1.0", extension = "zip") === Resource.parse("  org   foo   1.0 type:zip "))

    List("org foo", "org", "", "org foo 2.0 extra-term", "org foo 1.0 bad-key:34").foreach{
      s => 
        try {
          Resource.parse(s)
          fail(s + " should have failed to parse")
        } catch {
          case _ : RuntimeException => 
        }
    }
  }
}  
