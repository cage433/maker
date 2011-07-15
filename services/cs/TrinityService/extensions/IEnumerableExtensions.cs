using System.Linq;
using System.Text;

namespace System.Collections.Generic
{
    public static class IEnumerableExtensions
    {
        public static string Join<T>(this IEnumerable<T> enumerable, string sep)
        {
            var builder = new StringBuilder();

            foreach (var item in enumerable)
            {
                if (builder.Length > 0)
                {
                    builder.Append(sep);
                }

                builder.Append(item);
            }

            return builder.ToString();
        }

        public static T FirstOr<T>(this IEnumerable<T> enumerable, Func<T> alternative)
        {
            return enumerable.Count() > 0 ? enumerable.First(item => true) : alternative();
        }
    }
}