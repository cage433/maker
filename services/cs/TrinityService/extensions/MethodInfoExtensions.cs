using System.Collections.Generic;
using System.Linq;
using com.trafigura.services.util;

namespace System.Reflection
{
    public static class MethodInfoExtensions
    {
        public static T Attribute<T>(this MethodInfo method) where T : Attribute
        {
            return method.GetCustomAttributes(typeof(T), false)[0] as T;
        }

        public static Option<T> OptionalAttribute<T>(this MethodInfo method) where T : Attribute
        {
            return method.HasAttribute<T>() ? method.Attribute<T>().ToOption() : new None<T>();
        }

        public static bool HasAttribute<T>(this MethodInfo method) where T : Attribute
        {
            return method.GetCustomAttributes(typeof(T), true).ToList().Count() > 0;
        }
    }
}