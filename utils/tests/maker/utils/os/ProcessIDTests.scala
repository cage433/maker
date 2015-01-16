package maker.utils.os

import org.scalatest.FunSuite

class ProcessIDTests extends FunSuite{

  test("Can construct a process id"){
    val p = ProcessID()
      assert(p.id > 0)
  }
}
