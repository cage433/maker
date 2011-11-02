package starling.utils

import org.jmock.{Expectations, Mockery}
import org.hamcrest.Matcher
import org.jmock.api.Action
import org.scalatest.{Suite, BeforeAndAfter}
import collection.mutable.Stack
import org.jmock.internal.{ExpectationBuilder, ExpectationCollector}
import org.jmock.syntax.{ActionClause, ArgumentConstraintPhrases, CardinalityClause}

trait JMocker extends BeforeAndAfter { self: Suite =>
  private val (adaptingContext, adaptingExpectations) = (new AdaptingJMockery, new AdaptingExtendedExpectations)
  protected val (context, expectations): (JMockery, ExtendedExpectations) = (adaptingContext, adaptingExpectations)

  before { adaptingContext.push(new JMockUtil) }
  after  { adaptingContext.pop                 }

  trait JMockery {
    def mock[T <: AnyRef](implicit manifest: Manifest[T]): T
    def expecting(f: => Any)
    def whenExecuting[T](fun: => T): T
  }

  class AdaptingJMockery extends JMockery {
    def push(adapted: JMockery) = stack.push(adapted)
    def pop = stack.pop
    def mock[T <: AnyRef](implicit manifest: Manifest[T]): T = adapted.mock[T]
    def expecting(f: => Any) = adapted.expecting(f)
    def whenExecuting[T](fun: => T) = adapted.whenExecuting(fun)

    private val stack = new Stack[JMockery]
    private def adapted = stack.top
  }

  class JMockUtil extends JMockery {
    private val context = new Mockery

    def mock[T <: AnyRef](implicit manifest: Manifest[T]): T = context.mock(manifest.erasure.asInstanceOf[Class[T]])
    def expecting(f: => Any) = adaptingExpectations.withExpectations(new JMockerExpectations) { e =>
      f
      context.checking(e)
    }

    def whenExecuting[T](fun: => T): T = {
      val res = fun
      context.assertIsSatisfied()
      res
    }
  }

  trait ExtendedExpectations extends ExpectationBuilder with CardinalityClause with ArgumentConstraintPhrases with ActionClause {
    def any[T <: AnyRef](implicit manifest: Manifest[T]): T
    def oneOf[T](mockObject: T): T

    def withArg[T](value: T): T
    def withArg(value: Int): Int
    def withArg(value: Short): Short
    def withArg(value: Byte): Byte
    def withArg(value: Long): Long
    def withArg(value: Boolean): Boolean
    def withArg(value: Float): Float
    def withArg(value: Double): Double
    def withArg(value: Char): Char
    def withArg[T](matcher: Matcher[T]): T
    def withArg(matcher: Matcher[Int]): Int
    def withArg(matcher: Matcher[Short]): Short
    def withArg(matcher: Matcher[Byte]): Byte
    def withArg(matcher: Matcher[Long]): Long
    def withArg(matcher: Matcher[Boolean]): Boolean
    def withArg(matcher: Matcher[Float]): Float
    def withArg(matcher: Matcher[Double]): Double
    def withArg(matcher: Matcher[Char]): Char
  }

  class AdaptingExtendedExpectations extends ExtendedExpectations {
    def withExpectations[T](adapted: ExtendedExpectations)(f: ExtendedExpectations => T):T = {
      stack.push(adapted)
      val res = f(adapted)
      stack.pop
      res
    }

    def `with`(matcher: Matcher[java.lang.Double]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Boolean]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Byte]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Short]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Character]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Integer]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Long]) = adapted.`with`(matcher)
    def `with`(matcher: Matcher[java.lang.Float]) = adapted.`with`(matcher)
    def `with`[T](matcher: Matcher[T]) = adapted.`with`(matcher)

    def exactly(count: Int) = adapted.exactly(count)
    def atLeast(count: Int) = adapted.atLeast(count)
    def between(minCount: Int, maxCount: Int) = adapted.between(minCount, maxCount)
    def atMost(count: Int) = adapted.atMost(count)
    def one[T](mockObject: T) = adapted.one(mockObject)
    def oneOf[T](mockObject: T) = adapted.oneOf(mockObject)
    def allowing[T](mockObject: T) = adapted.allowing(mockObject)
    def allowing(mockObjectMatcher: Matcher[_]) = adapted.allowing(mockObjectMatcher)
    def ignoring[T](mockObject: T) = adapted.ignoring(mockObject)
    def ignoring(mockObjectMatcher: Matcher[_]) = adapted.ignoring(mockObjectMatcher)
    def never[T](mockObject: T) = adapted.never(mockObject)
    def will(action: Action) = adapted.will(action)
    def any[T <: AnyRef](implicit manifest: Manifest[T]) = adapted.any[T]
    def withArg[T](value: T) = adapted.withArg(value)
    def withArg(value: Int) = adapted.withArg(value)
    def withArg(value: Short) = adapted.withArg(value)
    def withArg(value: Byte) = adapted.withArg(value)
    def withArg(value: Long) = adapted.withArg(value)
    def withArg(value: Boolean) = adapted.withArg(value)
    def withArg(value: Float) = adapted.withArg(value)
    def withArg(value: Double) = adapted.withArg(value)
    def withArg(value: Char) = adapted.withArg(value)
    def withArg[T](matcher: Matcher[T]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Int]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Short]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Byte]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Long]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Boolean]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Float]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Double]) = adapted.withArg(matcher)
    def withArg(matcher: Matcher[Char]) = adapted.withArg(matcher)
    def buildExpectations(defaultAction: Action, collector: ExpectationCollector) = adapted.buildExpectations(defaultAction, collector)

    private val stack = new Stack[ExtendedExpectations]
    private def adapted = stack.top
  }

  class JMockerExpectations extends Expectations with ExtendedExpectations {
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