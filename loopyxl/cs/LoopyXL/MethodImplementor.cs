using System.Reflection.Emit;

namespace LoopyXL
{
    public interface MethodImplementor
    {
        IOption<DynamicMethod> ImplementMethod(Method method);
        DynamicMethod ImplementMethodUnsafe(Method method);
    }
}