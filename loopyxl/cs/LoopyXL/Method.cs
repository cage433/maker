using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using log4net;
using loopyxl;

namespace LoopyXL
{
    public class Method
    {
        private static readonly ILog log = LogManager.GetLogger(typeof(Method));

        public static IOption<Method> Create(MethodDefinition definition)
        {
            try
            {
                log.Info("Create: " + definition.name);

                return new Some<Method>(new Method(definition));
            }
            catch (Exception e)
            {
                log.Info("Epic fail: " + e.Message);

                return new None<Method>();
            }
        }

        public Method(MethodDefinition definition)
        {
            this.Definition = definition;
            ReturnType = TypeLookup.GetType(definition.returnType);
            ParameterTypes = definition.parameterTypes.Select(parameterType => TypeLookup.GetType(parameterType)).ToArray();
        }

        public MethodDefinition Definition { get; private set; }
        public int Id { get { return Definition.id; } }

        public string Name { get { return Definition.name; } }
        public Type ReturnType { get; private set; }
        public Type[] ParameterTypes { get; private set; }

        public bool Equals(Method other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Equals(other.Name, Name) && Equals(other.ReturnType, ReturnType) && Equals(other.ParameterTypes, ParameterTypes);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof (Method)) return false;
            return Equals((Method) obj);
        }

        public override int GetHashCode()
        {
            return Id;
        }
    }

    public interface IOption<T> : IEnumerable<T>
    {
        T GetOrThrow(Func<Exception> action);
        T GetOrElse(T alternative);
    }

    public class Some<T> : IOption<T>
    {
        private readonly T value;

        public Some(T value)
        {
            this.value = value;
        }

        public IEnumerator<T> GetEnumerator()
        {
            yield return value;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public T GetOrThrow(Func<Exception> action)
        {
            return value;
        }

        public T GetOrElse(T alternative)
        {
            return value;
        }
    }

    public class None<T> : IOption<T>
    {
        public IEnumerator<T> GetEnumerator()
        {
            yield break;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public T GetOrThrow(Func<Exception> action)
        {
            throw action();
        }

        public T GetOrElse(T alternative)
        {
            return alternative;
        }
    }
}