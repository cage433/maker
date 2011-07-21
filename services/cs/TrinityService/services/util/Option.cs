using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace com.trafigura.services.util
{
    public interface Option<out T> : IEnumerable<T>
    {
        V Fold<V>(Func<V> none, Func<T, V> some);
    }

    public class None<T> : Option<T>
    {
        public V Fold<V>(Func<V> none, Func<T, V> some)
        {
            return none();
        }

        public IEnumerator<T> GetEnumerator()
        {
            return Enumerable.Empty<T>().GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    public class Some<T> : Option<T>
    {
        private readonly T value;

        public Some(T value)
        {
            this.value = value;
        }

        public V Fold<V>(Func<V> none, Func<T, V> some)
        {
            return some(value);
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new List<T> {value}.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}