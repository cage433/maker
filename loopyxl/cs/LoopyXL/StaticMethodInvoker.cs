using System;
using log4net;

namespace LoopyXL
{
    public class StaticMethodInvoker
    {
        private static readonly ILog log = LogManager.GetLogger(typeof (StaticMethodInvoker));
        private static MethodInvoker methodInvoker = new MissingMethodInvoker("Marshaller missing");

        public static MethodInvoker MethodInvoker
        {
            set { methodInvoker = value; }
        }

        public static object Invoke(int methodId, params object[] args)
        {
            return methodInvoker.Invoke(methodId, args);
        }

        public static void Log(string toLog)
        {
            Console.WriteLine(toLog);
            log.Info(toLog);
        }

        public static double Echo(double value)
        {
            Console.WriteLine("Echo: " + value);
            log.Info("Echo: " + value);

            return value;
        }
    }
}