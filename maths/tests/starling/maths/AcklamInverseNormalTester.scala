package starling.maths


import org.testng.annotations.Test
import org.testng.Assert

class AcklamInverseNormalTester{

  @Test
  def test{
    for (x <- List(0.001, 0.1, 0.345678,  0.5, 0.9, 0.9999)){
      val y = AcklamInverseNormal.invoke(x)
      val z = RandomVariables.standardNormal(1234).cdf(y)
      Assert.assertEquals(z, x, 1e-5)
    }
  }
}
