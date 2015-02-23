package maker.utils

import org.scalatest.FunSpec

class TimingsTests extends FunSpec {
  describe("Concurrency Counter"){

    it("Should return Nil when passed an empty list"){
      assert(Timings(Nil).countConcurrency === Nil)
    }

    it("Should return a single element list when passed one"){
      assert(Timings(List((0l, 1l))).countConcurrency === List((0l, 1l, 1)))
    }

    it("Should break up two overlapping periods into three"){
      val times = List((0l, 2l), (1l, 3l))
      val expected = List((0l, 1l, 1), (1l, 2l, 2), (2l, 3l, 1))
      assert(Timings(times).countConcurrency === expected)
    }

    it("Should leave two non-overlapping periods alone"){
      val times = List((0l, 1l), (2l, 3l))
      val expected = List((0l, 1l, 1), (2l, 3l, 1))
      assert(Timings(times).countConcurrency === expected)
    }

    it("Should convert two touching periods into two"){
      val times = List((0l, 1l), (1l, 2l))
      val expected = List((0l, 1l, 1), (1l, 2l, 1))
      assert(Timings(times).countConcurrency === expected)
    }

    it("Should convert several periods as expected"){
      val times = List(
        (0l,  1l),
        (     1l,     3l),
        (         2l,     4l),
        (         2l,     4l),
        (                 4l,  5l),
        (                          6l, 7l)
      )
      val expected = List(
        (0l,  1l,                             1),
        (     1l, 2l,                         1),
        (         2l, 3l,                     3),
        (         3l,     4l,                 2),
        (                 4l,  5l,            1),
        (                          6l, 7l,    1)
      )
      assert(Timings(times).countConcurrency === expected)
    }

    it("Should handle multiple tests starting and finishing at the same time"){
      val times = List(
        (0l,          3l),
        (     1l,     3l),
        (         2l, 3l),
        (         2l,     4l),
        (             3l,      5l),
        (                 4l,  5l)
      )
      val expected = List(
        (0l,  1l,                             1),
        (     1l, 2l,                         2),
        (         2l, 3l,                     4),
        (             3l, 4l,                 2),
        (                 4l,  5l,            2)
      )
      assert(Timings(times).countConcurrency === expected)
    }
  }
}
