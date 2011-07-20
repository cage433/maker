using System;
using System.Collections.Generic;
using System.Linq;
using System.Web.Routing;
using com.trafigura.services.util;
using log4net;
using Microsoft.ApplicationServer.Http.Description;
using RouteCollectionExtensions = Microsoft.ApplicationServer.Http.Activation.RouteCollectionExtensions;

namespace com.trafigura.services
{
    public class RouteRegistry
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(RouteRegistry));

        private readonly IDictionary<string, Type> serviceTypes = new Dictionary<string,Type>();
        private readonly SimpleResourceFactory resourceFactory;
        private readonly IHttpHostConfigurationBuilder config;

        public RouteRegistry(SimpleResourceFactory resourceFactory, IHttpHostConfigurationBuilder config)
        {
            this.resourceFactory = resourceFactory;
            this.config = config;
        }

        public RouteRegistry MapServiceRoute<T>(string routePrefix, T service)
        {
            serviceTypes["/" + routePrefix] = typeof(T);
            resourceFactory.Services[typeof(T)] = service;

            RouteCollectionExtensions.MapServiceRoute<T>(RouteTable.Routes, routePrefix, config);

            return this;
        }

        public Option<KeyValuePair<string, Type>> GetServiceTypeForUri(Func<string, bool> predicate)
        {
            var matchingRoutes = serviceTypes.Keys.Where(predicate).OrderBy(key => key.Length).ToList();

            if (matchingRoutes.Count > 0)
            {
                var matchingRoute = matchingRoutes.Last();

                return new KeyValuePair<string, Type>(matchingRoute, serviceTypes[matchingRoute]).ToOption();
            }
            else
            {
                return new None<KeyValuePair<string, Type>>();
            }
        }
    }
}