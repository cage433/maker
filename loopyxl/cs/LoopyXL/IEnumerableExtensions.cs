using System.Linq;
using System.Text;
using LoopyXL;

namespace System.Collections.Generic
{
    public static class IEnumerableExtensions
    {
        public static IEnumerable<T> Flatten<T>(this IEnumerable<IOption<T>> items)
        {
            return items.SelectMany(item => item);
        }

        public static IEnumerable<T> Flatten<T>(this IEnumerable<IEnumerable<T>> items)
        {
            return items.SelectMany(item => item);
        }

        public static IEnumerable<V> Select<T, U, V>(this IEnumerable<KeyValuePair<T, U>> items, Func<T, U, V> func)
        {
            return items.Select(keyValue => func(keyValue.Key, keyValue.Value));
        }

        public static IEnumerable<KeyValuePair<T, U>> Zip<T, U>(this IEnumerable<T> ts, IEnumerable<U> us)
        {
            var tEnumerator = ts.GetEnumerator();
            var uEnumerator = us.GetEnumerator();

            while (tEnumerator.MoveNext() && uEnumerator.MoveNext())
            {
                yield return new KeyValuePair<T, U>(tEnumerator.Current, uEnumerator.Current);
            }
        }

        public static String Concat<T>(this IEnumerable<T> enumerable, string separator)
        {
            var builder = new StringBuilder();

            foreach (var value in enumerable)
            {
                if (builder.Length != 0)
                {
                    builder.Append(separator);
                }

                builder.Append(value);
            }

            return builder.ToString();
        }
    }
}