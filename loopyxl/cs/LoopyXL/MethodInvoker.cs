namespace LoopyXL
{
    public interface MethodInvoker
    {
        object Invoke(int methodId, params object[] args);
    }
}