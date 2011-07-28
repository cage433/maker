using System.Reflection;

namespace LoopyXL
{
    public interface MethodInfoRegistry
    {
        void RegisterMethods(params MethodInfo[] methods);
    }
}