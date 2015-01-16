package maker.utils

import org.scalatest.FunSuite
import java.io.File
import FileUtils._

class FileUtilsTests extends FunSuite{
  test("With temp dir"){
    var savedDir : File = null
    FileUtils.withTempDir{
      dir =>
        savedDir = dir
        assert(dir.exists)
    }
    assert(! savedDir.exists)
  }

  test("relative files"){
    assert(file("/a/b/c/d").relativeTo(file("/a/b")) === file("c/d"))
    assert(file("/a/b/c/d").relativeTo(file("/a/b/")) === file("c/d"))
    assert(file("/a/b/c/d").relativeTo(file("/a/b/x")) === file("../c/d"))
    assert(file("launcher").relativeTo(file(".")) === file("launcher"))
  }

  test("clean files leaves directories alone"){
    withTempDir{
      dir => 
       val fred = file(dir, "fred")
       fred.createNewFile
       val subDir = file(dir, "subdir")
       subDir.mkdir
       val mike = file(subDir, "mike")
       mike.createNewFile
       assert(fred.exists)
       assert(mike.exists)
       assert(subDir.exists)
       cleanRegularFilesLeavingDirectories(dir)
       assert(!fred.exists)
       assert(!mike.exists)
       assert(subDir.exists)
    }
  }

}
