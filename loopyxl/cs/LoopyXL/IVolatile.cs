using System;

namespace LoopyXL
{
    public interface IVolatile<T>
    {
        T Value { get; }

        event Action Invalidated;
    }
}