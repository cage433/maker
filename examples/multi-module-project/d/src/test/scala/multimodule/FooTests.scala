package multimodule

import org.scalatest.FunSpec
import java.io.File

class FooTests extends FunSpec {
  describe("A foo"){
    it("Should work"){
      val foo = Foo(10)
      assert(foo.x === 10)
    }


    /* 
      A slight abuse of the testing framework - however to be certain
      that the tests actually run, and don't return success without having
      done anything, the tests will be called from a script like
      
      touch marker-file
      bin/maker.sh -E 'project.test'
      if [ -e marker-file ]; then
        exit -1
      else 
        exit 0
      fi
    */
    it("Should delete a file we created earlier"){
      val markerFile = new File("marker-file")
      assert(markerFile.exists, "Marker file should exist")
      markerFile.delete
      assert(! markerFile.exists, "Marker file should no longer exist")
    }
  }
}
