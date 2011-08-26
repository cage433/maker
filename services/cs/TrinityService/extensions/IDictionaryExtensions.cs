namespace System.Collections.Generic
{
    public static class IDictionaryExtensions
    {
        public static V GetOrInitialise<K, V>(this IDictionary<K, V> dictionary, K key, Func<K, V> values)
        {
            V value;
            if (!dictionary.TryGetValue(key, out value))
            {
                value = dictionary[key] = values(key);
            }

            return value;
        }

        public static V GetOrElse<K, V>(this IDictionary<K, V> dictionary, K key, V alternative)
        {
            V value;

            return dictionary.TryGetValue(key, out value) ? value : alternative;
        }

        public static V GetOrElse<K, V>(this IDictionary<K, V> dictionary, K key, Func<V> alternative)
        {
            V value;

            return dictionary.TryGetValue(key, out value) ? value : alternative();
        }
    }
}