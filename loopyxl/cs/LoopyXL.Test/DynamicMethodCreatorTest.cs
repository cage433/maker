using Moq;
using NUnit.Framework;

namespace LoopyXL.Test
{
    [TestFixture]
    public class DynamicMethodCreatorTest
    {
        // Unit under test
        private DynamicMethodImplementor dynamicMethodImplementor;

        #region Setup
        // Collaborator
        private Mock<MethodInvoker> methodInvoker;

        // Data
        private readonly Method doubleMethod = 
            new Method(TestHelper.CreateMethodDefinition("DoubleMethod", "double", "double"));

        private readonly Method stringMethod =
            new Method(TestHelper.CreateMethodDefinition("StringMethod", "string", "string"));

        private readonly Method stringMatrixMethod = 
            new Method(TestHelper.CreateMethodDefinition("StringMatrixMethod", "string[][]", "string[][]"));

        private readonly Method twoDoubleArraysMethod =
            new Method(TestHelper.CreateMethodDefinition("TwoDoubleArraysMethod", "double", "double[]", "double[]"));

        [SetUp]
        public void SetUp()
        {
            methodInvoker = new Mock<MethodInvoker>(MockBehavior.Strict);

            StaticMethodInvoker.MethodInvoker = methodInvoker.Object;

            dynamicMethodImplementor = new DynamicMethodImplementor();
        }

        [TearDown]
        public void TearDown()
        {
            methodInvoker.VerifyAll();
        }

        #endregion

        [Test]
        public void ShouldRedirectInvocationToMethodInvoker()
        {
            AssertRoutesInvocationToMethodInvoker(stringMethod, "output", "output", new object[]{"input"});
            AssertRoutesInvocationToMethodInvoker(doubleMethod, 456.0, 456.0, new object[]{1.2});
            var strings = new[,] { { "input" } };
            AssertRoutesInvocationToMethodInvoker(stringMatrixMethod, new[,] { { "output" } }, new[,] { { "output" } }, 
                new object[]{strings});
            AssertRoutesInvocationToMethodInvoker(twoDoubleArraysMethod, 456.0, 456.0,
                new object[] { new[] {123.4}, new[] {456.7} });
        }

        private void AssertRoutesInvocationToMethodInvoker(Method method, object output, object parsedOutput, object[] input)
        {
            var dynamicMethod = dynamicMethodImplementor.ImplementMethodUnsafe(method);

            methodInvoker.Setup(it => it.Invoke(method.Id, input))
                .Returns(ValueFunction(output));

            object result = dynamicMethod.Invoke(null, input);

            Assert.AreEqual(parsedOutput, result);

            methodInvoker.Verify(it => it.Invoke(method.Id, input));
        }

        private object ValueFunction(object output)
        {
            return output;
        }
    }
}