package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import collection.mutable.Stack


class RetryingActionTests extends WordSpec with ShouldMatchers {
  "Retrying action should retry until the first success" in {
    val results = new Stack[() => String]() ++
      List(failure("failure 1"), failure("failure 2"), success("success 1"), success("success 2"), failure("failure 3"))

    val action = () => results.pop()()
    val retryingAction = new RetryingAction[String](action)

    intercept[Exception](retryingAction()).getMessage should be === "failure 1"
    intercept[Exception](retryingAction()).getMessage should be === "failure 2"
    retryingAction() should be === "success 1"
    retryingAction() should be === "success 1"
  }

  private def success[T](result: T): () => T = () => result
  private def failure[T](message: String): () => T = () => throw new Exception(message)
}