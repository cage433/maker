using System.Net;
using System.Net.Http;
using System.Web.Hosting;
using com.trafigura.services;
using com.trafigura.services.example;
using com.trafigura.services.trinity;
using com.trafigura.services.util;
using log4net;
using Microsoft.ApplicationServer.Http;
using Microsoft.ApplicationServer.Http.Description;

namespace com.trafigura
{
    public class RouteRegistraar
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(RouteRegistraar));
        private readonly bool IsDevelopment = Dns.GetHostName() == "LON-SCURLDT";

        public readonly SimpleResourceFactory ResourceFactory;
        public readonly IHttpHostConfigurationBuilder Config;
        private readonly RouteRegistry RouteRegistry;

        public RouteRegistraar()
        {
            ResourceFactory = new SimpleResourceFactory();
            Config = HttpHostConfiguration.Create()
                .SetResourceFactory(ResourceFactory)
                .SetMessageHandlerFactory(CreateChannel)
                .AddFormatters(new JsonFormatter(), new HtmlForamtter());
            RouteRegistry = new RouteRegistry(ResourceFactory, Config);
        }

        public RouteRegistry Register()
        {
            RouteRegistry.MapServiceRoute("Doc", new DocumentationService(RouteRegistry));
            RouteRegistry.MapServiceRoute("Example", new ExampleService());

            if (!IsDevelopment)
            {
                new TrinityServices(new TrinityCredentials("TRUAT", "STARLING", "trinity")).MapServiceRoues(RouteRegistry);
            }

            return RouteRegistry;
        }

        private LoggingChannel CreateChannel(HttpMessageChannel channel)
        {
            return new LoggingChannel(new DocumentationChannel(HostingEnvironment.ApplicationVirtualPath ?? "/", RouteRegistry,
                new UriFormatExtensionMessageChannel(channel).AddMapping("json", "application/json")));
        }
    }
}