using System;
using System.Threading;
using log4net;

namespace LoopyXL
{
    public class Volatile<T> : IVolatile<T>
    {
        public Volatile(T value, Func<bool> isValid)
        {
            this.Value = value;

            new BackgroundTask(() =>
            {
                while (isValid())
                {
                    Thread.Sleep(500);
                }

                Invalidated();
            }).Start();
        }

        public T Value { get; private set; }

        public event Action Invalidated;
    }
}