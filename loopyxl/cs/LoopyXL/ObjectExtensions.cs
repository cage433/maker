using System.Collections.Generic;

namespace System
{
    public static class ObjectExtensions
    {
        public static T AssertNotNull<T>(this T value, String message) where T : class
        {
            if (value == null)
            {
                throw new Exception(message);
            }

            return value;
        }

        public static T Update<T>(this T value, Action<T> action)
        {
            action(value);

            return value;
        }
    }
}