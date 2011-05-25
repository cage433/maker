using NUnit.Framework;

namespace LoopyXL.Test
{
    public class TypedMatrixMarshallerTest
    {
        // Unit under test
        private readonly TypedMatrixMarshaller marshaller = new TypedMatrixMarshaller();

        [Test]
        public void FromIsInverseOfTo()
        {
            object[,] input = new object[,] {{1.2, "foo"}, {3.4, "oof"}};
            object[,] output = marshaller.To(marshaller.From(input));

            TestHelper.AssertMatrixesAreEqual(input, output);
        }
    }
}