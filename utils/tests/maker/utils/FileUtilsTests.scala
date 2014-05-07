/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
