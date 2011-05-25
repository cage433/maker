using System;

namespace LoopyXL
{
    public class MissingMethodInvoker : MethodInvoker
    {
        private readonly string reason;

        public MissingMethodInvoker(string reason)
        {
            this.reason = reason;
        }

        public object Invoke(int methodId, params object[] args)
        {
            throw new MarshallException(reason);
        }
    }
}