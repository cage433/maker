using loopyxl;
using NUnit.Framework;

namespace LoopyXL.Test
{
    public static class TestHelper
    {
        public static MethodDefinition CreateMethodDefinition(string name, string returnType, params string[] parameterTypes)
        {
            var methodDefinition = new MethodDefinition { name = name, returnType = returnType };

            methodDefinition.parameterTypes.AddRange(parameterTypes);

            return methodDefinition;
        }

        public static void AssertMatrixesAreEqual<T>(T[,] input, T[,] output)
        {
            Assert.IsNotNull(output);
            Assert.AreEqual(input.GetLength(0), output.GetLength(0));
            Assert.AreEqual(input.GetLength(1), output.GetLength(1));

            for (int i = 0; i < input.GetLength(0); i++)
            {
                for (int j = 0; j < input.GetLength(1); j++)
                {
                    Assert.AreEqual(input[i, j], output[i, j]);
                }
            }
        }
    }
}