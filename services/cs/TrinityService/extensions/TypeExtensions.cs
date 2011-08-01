using System.Collections.Generic;

namespace System
{
    public static class TypeExtensions
    {
        public static string CodeString(this Type type, bool shortName = false)
        {
            var name = shortName ? type.Name : type.FullName;

            if (!type.IsGenericType)
            {
                return name;
            }

            string value = name.Substring(0, name.IndexOf('`')).Substring(type.Namespace.Length + 1) + "<";
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
}