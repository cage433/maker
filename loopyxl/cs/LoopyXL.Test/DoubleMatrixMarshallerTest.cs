using NUnit.Framework;

namespace LoopyXL.Test
{
    [TestFixture]
    public class DoubleMatrixMarshallerTest
    {
        // Unit under test
        private readonly DoubleMatrixMarshaller marshaller = new DoubleMatrixMarshaller();

        [Test]
        public void FromIsInverseOfTo()
        {
            double[,] input = new double[,] {{1.2, 2.3}, {4.5, 5.6}, {6.7, 7.8}};

            double[,] output = marshaller.To(marshaller.From(input));

            TestHelper.AssertMatrixesAreEqual(input, output);
        }
    }
}