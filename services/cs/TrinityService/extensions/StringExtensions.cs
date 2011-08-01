using System.Collections.Generic;
using System.Linq;
using com.trafigura.services.util;
using Newtonsoft.Json.Linq;

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

        public static JToken ParseJson(this string edmJson)
        {
            return edmJson.Trim().StartsWith("[") ? (JToken)JArray.Parse(edmJson) : JObject.Parse(edmJson);
        }

        public static int ParseDate(this string date)
        {
            return (int) DateTime.Parse(date).ToOADate();
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