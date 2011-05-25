using System.Collections.Generic;

namespace LoopyXL
{
    public interface MethodProvider
    {
        IList<Method> GetMethods();
    }

    public static class MethodProviderExtensions
    {
        public static CachingMethodProvider Cache(this MethodProvider toCache)
        {
            return new CachingMethodProvider(toCache);
        }
    }
}