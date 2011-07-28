using System;
using log4net.Appender;
using log4net.Core;

namespace LoopyXL
{
    public class ExcelDNAAppender : IAppender
    {
        public string Name { get; set; }

        public void DoAppend(LoggingEvent loggingEvent)
        {
            ExcelDna.Logging.LogDisplay.WriteLine(AsString(loggingEvent));
        }

        private string AsString(LoggingEvent loggingEvent)
        {
            String value = string.Format("{0} : {1}", loggingEvent.LoggerName, loggingEvent.MessageObject);

            if (loggingEvent.ExceptionObject != null)
            {
                Exception exception = loggingEvent.ExceptionObject;

                value = value + string.Format(": {0} : {1}", exception.Message, exception);
            }

            return value;
        }

        public void Close()
        {
        }
    }
}