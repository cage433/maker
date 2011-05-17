using System.Collections.Generic;

namespace LoopyXL
{
    public static class IDictionaryExtensions
    {
        public static V GetOrElse<K, V>(this IDictionary<K, V> dictionary, K key, V alternative)
        {
            return dictionary.ContainsKey(key) ? dictionary[key] : alternative;
        }
    }
}