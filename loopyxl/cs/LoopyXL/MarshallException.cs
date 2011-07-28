using System;

namespace LoopyXL
{
    public class MarshallException : Exception
    {
        public MarshallException(string message, params object[] args)
            : base(String.Format(message, args))
        {
        }

        public MarshallException(Exception cause, string message, params object[] args)
            : base(String.Format(message, args), cause)
        {
        }
    }
}