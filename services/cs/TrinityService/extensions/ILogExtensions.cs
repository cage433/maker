using System;

namespace log4net
{
    public static class ILogExtensions
    {
        public static void Info(this ILog logger, string message, Action action)
        {
            Info(logger, message, () => { action(); return true; });
        }

        public static T Info<T>(this ILog logger, string message, Func<T> action)
        {
            logger.Info(message + " ...");

            try
            {
                T result = action();

                logger.Info(message + " DONE");

                return result;
            }
            catch (Exception e)
            {
                logger.Error(message + " FAILED", e);

                throw;
            }
        }
    }
}