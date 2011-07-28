using loopyxl;

using System.Linq;

namespace LoopyXL
{
    public class DoubleMatrixMarshaller
    {
        public InvocationValue From(double[,] parameters)
        {
            var result = new InvocationValue { type = InvocationValue.Type.DOUBLE_MATRIX };

            var width = parameters.GetLength(0);

            for (int i = 0; i < width; i++)
            {
                int height = parameters.GetLength(1);

                var doubleArray = new DoubleArray();

                for (int j = 0; j < height; j++)
                {
                    doubleArray.values.Add(parameters[i, j]);
                }

                result.doubleMatrix.Add(doubleArray);
            }

            return result;
        }

        public double[,] To(InvocationValue value)
        {
            int height = value.doubleMatrix.Count;
            int width = height == 0 ? 0 : value.doubleMatrix[0].values.Count;

            var result = new double[height, width];

            for (int i = 0; i < height; i++)
            {
                DoubleArray array = value.doubleMatrix[i];

                for (int j = 0; j < width; j++)
                {
                    result[i, j] = array.values[j];
                }
            }

            return result;
        }
    }

    public class StringMatrixMarshaller
    {
        public InvocationValue From(string[,] parameters)
        {
            var result = new InvocationValue { type = InvocationValue.Type.STRING_MATRIX };

            var width = parameters.GetLength(0);

            for (int i = 0; i < width; i++)
            {
                int height = parameters.GetLength(1);

                var stringArray = new StringArray();

                for (int j = 0; j < height; j++)
                {
                    stringArray.values.Add(parameters[i, j]);
                }

                result.stringMatrix.Add(stringArray);
            }

            return result;
        }

        public string[,] To(InvocationValue value)
        {
            int height = value.stringMatrix.Count;
            int width = height == 0 ? 0 : value.stringMatrix[0].values.Count;

            var result = new string[height, width];

            for (int i = 0; i < height; i++)
            {
                StringArray array = value.stringMatrix[i];

                for (int j = 0; j < width; j++)
                {
                    result[i, j] = array.values[j];
                }
            }

            return result;
        }
    }
}