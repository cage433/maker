package single

import org.scalatest.FunSpec

class FooTests extends FunSpec {
  describe("A Foo"){
    it("Should behave nicely"){
      val foo = Foo(10)
      assert(foo.x === 10)
    }
  }
}
