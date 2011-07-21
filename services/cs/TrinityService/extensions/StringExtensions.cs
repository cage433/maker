using System.Collections.Generic;
using System.Linq;
using com.trafigura.services.util;

namespace System
{
    public static class StringExtensions
    {
        public static string StripSuffix(this string input, string suffix)
        {
            return input.EndsWith(suffix) ? input.Substring(0, input.Length - suffix.Length) : input;
        }

        public static string DropUntil(this string input, char match)
        {
            return DropUntil(input, c => c == match);
        }

        public static string DropUntil(this string input, Func<char, bool> predicate)
        {
            return input.SkipWhile(character => !predicate(character)).Select(c => c.ToString()).Join("");
        }

        public static string Decapitalize(this string input)
        {
            var charArray = input.ToCharArray();
            charArray[0] = charArray[0].ToString().ToLower().ToCharArray()[0];
            return new String(charArray);
        }
    }

    public static class TypeExtensions
    {
        public static string CodeString(this Type type)
        {
            return CodeString(type, false);
        }

        public static string CodeString(this Type type, bool shortName)
        {
            var name = shortName ? type.Name : type.FullName;

            if (!type.IsGenericType)
            {
                return name;
            }

            string value = name.Substring(0, name.IndexOf('`')) + "<";
            Type[] genericArgs = type.GetGenericArguments();
            List<string> list = new List<string>();
            for (int i = 0; i < genericArgs.Length; i++)
            {
                value += "{" + i + "},";
                string s = CodeString(genericArgs[i], shortName);
                list.Add(s);
            }
            value = value.TrimEnd(',');
            value += ">";
            value = string.Format(value, list.ToArray());
            return value;
        }
    }

    public static class ObjectExtensions
    {
        public static Option<T> ToOption<T>(this T obj)
        {
            return obj == null ? (Option<T>) new None<T>() : new Some<T>(obj);
        }
    }
}