using NUnit.Framework;

namespace LoopyXL.Test
{
    public class TypedArrayMarshallerTest
    {
        // Unit under test
        private readonly TypedArrayMarshaller marshaller = new TypedArrayMarshaller();

        [Test]
        public void FromIsInverseOfTo()
        {
            object[] input = new object[] { 1.2, "foo", 3.4, "oof" };
            object[] output = marshaller.To(marshaller.From(input));

            Assert.IsNotNull(output);
            Assert.AreEqual(input.GetLength(0), output.GetLength(0));

            for (int i = 0; i < input.GetLength(0); i++)
            {
                Assert.AreEqual(input[i], output[i]);
            }
        }
    }
}