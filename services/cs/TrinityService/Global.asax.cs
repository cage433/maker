using log4net;
using System;

namespace com.trafigura
{
    public class Global : System.Web.HttpApplication
    {
        private static readonly ILog logger;

        static Global()
        {
            log4net.Config.XmlConfigurator.Configure();
            logger = LogManager.GetLogger(typeof(Global));
        }

        private readonly RouteRegistraar routeRegistraar;

        public Global()
        {
            routeRegistraar = new RouteRegistraar();
        }

        protected void Application_Start(object sender, EventArgs e)
        {
            logger.Info("Application_Start");

            routeRegistraar.Register();
        }
    }
}