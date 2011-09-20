package starling.utils

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class VerifyingDynamicProxyTests extends WordSpec with ShouldMatchers {
  "should complain when implementations differ" in {
    intercept[Exception] { invoke(1, 2)                                                         }
    intercept[Exception] { invoke(1, throw new RuntimeException("Boom !"))                             }
    intercept[Exception] { invoke(throw new RuntimeException("Boom !"), 2)                             }
    intercept[Exception] { create(throw new RuntimeException("Boom !"), throw new RuntimeException("Bang !")) }
  }

  "shouldn't complain when implementations are the same" in {
    invoke(1, 1) should be === 1

    try {
      invoke(throw new RuntimeException("Boom !"), throw new RuntimeException("Boom !"))
    } catch {
      case e => e.getMessage should be === "Boom !"
    }
  }

  private def invoke(canonicalResult: => Int, candidateResult: => Int): Int = {
    create(new Implementation(result = canonicalResult), new Implementation(result = candidateResult)).calc(1, "foo")
  }

  private def create(canonical: Trait, candidate: Trait) =
    VerifyingDynamicProxy.create[Trait](canonical, candidate, throwFailures = true)

  private trait Trait {
    def calc(a: Int, b: String): Int
  }

  private class Implementation(result: => Int) extends Trait {
    def calc(a: Int, b: String) = result
  }
}