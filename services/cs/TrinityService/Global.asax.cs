using System.Net;
using System.Net.Http;
using System.Web.Hosting;
using com.trafigura.services;
using com.trafigura.services.example;
using com.trafigura.services.trinity;
using com.trafigura.services.util;
using log4net;
using System;
using Microsoft.ApplicationServer.Http.Description;

namespace com.trafigura
{
    public class Global : System.Web.HttpApplication
    {
        private static readonly ILog logger;
        private static readonly bool IsDevelopment = Dns.GetHostName() == "LON-SCURLDT";

        static Global()
        {
            log4net.Config.XmlConfigurator.Configure();
            logger = LogManager.GetLogger(typeof(Global));
        }

        private readonly SimpleResourceFactory resourceFactory;
        private readonly IHttpHostConfigurationBuilder config;
        private readonly RouteRegistry routeRegistry;

        public Global()
        {
            resourceFactory = new SimpleResourceFactory();
            config = HttpHostConfiguration.Create()
                .SetResourceFactory(resourceFactory)
                .SetMessageHandlerFactory(CreateChannel)
                .AddFormatters(new JsonFormatter());
            routeRegistry = new RouteRegistry(resourceFactory, config);
        }

        protected void Application_Start(object sender, EventArgs e)
        {
            logger.Info("Application_Start");

            routeRegistry.MapServiceRoute("Example", new ExampleService());

            if (!IsDevelopment)
            {
                new TrinityServices(new TrinityCredentials("TRUAT", "STARLING", "trinity")).MapServiceRoues(routeRegistry);
            }
        }

        private LoggingChannel CreateChannel(HttpMessageChannel channel)
        {
            return new LoggingChannel(new DocumentationChannel(HostingEnvironment.ApplicationVirtualPath, routeRegistry, 
                new UriFormatExtensionMessageChannel(channel).AddMapping("json", "application/json")));
        }
    }
}