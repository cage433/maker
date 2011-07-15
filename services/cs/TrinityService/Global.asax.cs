using System.Net;
using com.trafigura.services.example;
using com.trafigura.services.trinity;
using com.trafigura.services.util;
using log4net;
using Microsoft.ApplicationServer.Http.Activation;

namespace ContactManager_Advanced
{
    using System;
    using System.Web.Routing;
    using Microsoft.ApplicationServer.Http.Description;

    public class Global : System.Web.HttpApplication
    {
        private static readonly ILog logger;

        static Global()
        {
            log4net.Config.XmlConfigurator.Configure();
            logger = LogManager.GetLogger(typeof (Global));
        }

        protected void Application_Start(object sender, EventArgs e)
        {
            logger.Info("Application_Start");

            var trinityCredentials = new TrinityCredentials
            {
                Database = "TRUAT",
                Username = "SCURL",
                Password = "trinity"
            };

            var resourceFactory = logger.Info("Constructing services", () =>
                IsDevelopment ? new SimpleResourceFactory(new ExampleService())
                              : new SimpleResourceFactory(new ExampleService(), new TrinityService(trinityCredentials)));

            var config = HttpHostConfiguration.Create()
                .SetResourceFactory(resourceFactory)
                .SetMessageHandlerFactory(channel => new LoggingChannel(
                    new UriFormatExtensionMessageChannel(channel).AddMapping("json", "application/json")))
                .AddFormatters(new JsonFormatter());

            RouteTable.Routes.MapServiceRoute<ExampleService>("Example", config);
            RouteTable.Routes.MapServiceRoute<TrinityService>("Trinity", config);
        }

        private static bool IsDevelopment
        {
            get { return Dns.GetHostName() == "LON-SCURLDT"; }
        }
    }
}