using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.ServiceModel.Web;
using System.Text;
using com.trafigura;
using com.trafigura.services;
using com.trafigura.services.meta;
using com.trafigura.services.util;
using log4net;
using Microsoft.ApplicationServer.Http.Activation;
using Newtonsoft.Json;
using JsonSerializer = com.trafigura.services.util.JsonSerializer;

namespace TrinityServiceConsole
{
    class Program
    {
        static void Main(string[] args)
        {
            TestJson();

            log4net.Config.XmlConfigurator.Configure();

            var logger = LogManager.GetLogger(typeof(Program));

            logger.Info("Running");

            var routeRegistraar = new RouteRegistraar();
            var routeRegistry = routeRegistraar.Register();

            var baseUri = new Uri("http://localhost:9100/");

            foreach (KeyValuePair<string, Type> serviceBinding in routeRegistry.ServiceTypes)
            {
                var host = new HttpConfigurableServiceHost(serviceBinding.Value, routeRegistraar.Config, 
                    new Uri(baseUri, serviceBinding.Key));

                host.Open();
            }

            Console.WriteLine("Press <ENTER> to terminate");
            Console.ReadLine();
        }

        private static void TestJson()
        {
            try
            {
                var jsonSerializer = new JsonSerializer(TypeNameHandling.Objects);
                var webMethodParameter = new WebMethodParameter {Binding = "B", Name = "N", ParameterType = "PT"};
                var param = jsonSerializer.Serialize(webMethodParameter);

                Console.WriteLine(param);

                var webMethod = new WebMethod
                {
                    Name = "N",
                    Parameters = new List<WebMethodParameter> { webMethodParameter, webMethodParameter },
                    ReturnType = "RT",
                    Uri = "U",
                    Verb = "V"
                };

                var method = jsonSerializer.Serialize(webMethod);

                Console.WriteLine(method);

                var service = jsonSerializer.Serialize(new WebService
                {
                    Uri = "U",
                    ServiceType = "ST",
                    Methods = new List<WebMethod> { webMethod, webMethod }
                });

                Console.WriteLine(service);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.StackTrace);
            }
        }
    }
}
