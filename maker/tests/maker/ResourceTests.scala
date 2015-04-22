package maker

import org.scalatest.FreeSpec

class ResourceTests extends FreeSpec {
  //"test url" in {
    //assert(Resource.parse("org.scalacheck scalacheck_2.9.2 1.9").relativeURL === "org/scalacheck/scalacheck_2.9.2/1.9/scalacheck_2.9.2-1.9.jar")
    //assert(Resource.parse("org.scalacheck scalacheck_2.9.2 1.9").basename === "org.scalacheck-scalacheck_2.9.2-1.9.jar")

    //val bookmarkDB = Resource("starling.test.resources", "bookmark-test-db", "1.2", extension = "gz")
    //assert(bookmarkDB.relativeURL === "starling/test/resources/bookmark-test-db/1.2/bookmark-test-db-1.2.gz")
    //assert(bookmarkDB.basename === "starling.test.resources-bookmark-test-db-1.2.gz")
  //}


  //"test string parsing" in {
    //assert(Resource("org", "foo", "1.0", extension = "jar") === Resource.parse("  org   foo   1.0 "))
    //assert(Resource("org", "foo", "1.0", extension = "zip") === Resource.parse("  org   foo   1.0 type:zip "))

    //List("org foo", "org", "", "org foo 2.0 extra-term", "org foo 1.0 bad-key:34").foreach{
      //s => 
        //try {
          //Resource.parse(s)
          //fail(s + " should have failed to parse")
        //} catch {
          //case _ : RuntimeException => 
        //}
    //}
  //}
}  
