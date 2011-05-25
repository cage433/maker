using NUnit.Framework;

namespace LoopyXL.Test
{
    [TestFixture]
    public class StringMatrixMarshallerTest
    {
        // Unit under test
        private readonly StringMatrixMarshaller marshaller = new StringMatrixMarshaller();
                
        [Test]
        public void FromIsInverseOfTo()
        {
            string[,] input = new string[,] { { "1.2", "foo" }, { "3.4", "oof" } };
            object[,] output = marshaller.To(marshaller.From(input));

            TestHelper.AssertMatrixesAreEqual(input, output);
        }
    }
}