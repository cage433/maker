package starling.utils

import org.jmock.{Expectations, Mockery}
import org.hamcrest.Matcher
import org.scalatest.fixture.FixtureFlatSpec


trait JMocker extends FixtureFlatSpec {
  type FixtureParam = JMockUtil
  def withFixture(test: OneArgTest) = test(new JMockUtil)

  class JMockUtil {
    private val context = new Mockery

    def mock[T <: AnyRef](implicit manifest: Manifest[T]): T = context.mock(manifest.erasure.asInstanceOf[Class[T]])
    def expecting(fun: JMockerExpectations => Unit) {
      val e = new JMockerExpectations
      fun(e)
      context.checking(e)
    }

    def whenExecuting(fun: => Unit) = {
      fun
      context.assertIsSatisfied()
    }
  }

  class JMockerExpectations extends Expectations {
    def any[T <: AnyRef](implicit manifest: Manifest[T]): T = `with`(Expectations.any(manifest.erasure.asInstanceOf[Class[T]]))

    def withArg[T](value: T): T = `with`(value)
    def withArg(value: Int): Int = `with`(value)
    def withArg(value: Short): Short = `with`(value)
    def withArg(value: Byte): Byte = `with`(value)
    def withArg(value: Long): Long = `with`(value)
    def withArg(value: Boolean): Boolean = `with`(value)
    def withArg(value: Float): Float = `with`(value)
    def withArg(value: Double): Double = `with`(value)
    def withArg(value: Char): Char = `with`(value)
    def withArg[T](matcher: Matcher[T]): T = `with`(matcher)
    def withArg(matcher: Matcher[Int]): Int = `with`(matcher)
    def withArg(matcher: Matcher[Short]): Short = `with`(matcher)
    def withArg(matcher: Matcher[Byte]): Byte = `with`(matcher)
    def withArg(matcher: Matcher[Long]): Long = `with`(matcher)
    def withArg(matcher: Matcher[Boolean]): Boolean = `with`(matcher)
    def withArg(matcher: Matcher[Float]): Float = `with`(matcher)
    def withArg(matcher: Matcher[Double]): Double = `with`(matcher)
    def withArg(matcher: Matcher[Char]): Char = `with`(matcher)
  }
}