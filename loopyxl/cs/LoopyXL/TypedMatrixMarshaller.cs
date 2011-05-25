using loopyxl;

namespace LoopyXL
{
    public class TypedMatrixMarshaller
    {
        private readonly TypedObjectMarshaller marshaller = new TypedObjectMarshaller();

        public InvocationValue From(object[,] parameters)
        {
            // TODO: Return an empty 'objectMatrix' when it only contains 'ExcelMissing' or 'ExcelEmpty'
            var value = new InvocationValue { type = InvocationValue.Type.OBJECT_MATRIX };

            for (int i = 0; i < parameters.GetLength(0); i++)
            {
                var typedArray = new TypedArray();

                for (int j = 0; j < parameters.GetLength(1); j++)
                {
                    typedArray.values.Add(marshaller.From(parameters[i, j]));
                }

                value.objectMatrix.Add(typedArray);
            }

            return value;
        }

        public object[,] To(InvocationValue value)
        {
            int height = value.objectMatrix.Count;
            int width = height == 0 ? 0 : value.objectMatrix[0].values.Count;

            var result = new object[height, width];

            for (int i = 0; i < height; i++)
            {
                for (int j = 0; j < width; j++)
                {
                    result[i, j] = marshaller.To(value.objectMatrix[i].values[j]);
                }
            }

            return result;
        }
    }
}