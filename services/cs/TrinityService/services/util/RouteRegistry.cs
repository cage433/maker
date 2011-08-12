using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.ServiceModel.Web;
using com.trafigura.services.meta;
using log4net;
using Microsoft.ApplicationServer.Http.Description;

namespace com.trafigura.services.util
{
    public class RouteRegistry
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(RouteRegistry));

        public readonly IDictionary<string, Type> ServiceTypes = new Dictionary<string, Type>();
        private readonly SimpleResourceFactory resourceFactory;
        private readonly IHttpHostConfigurationBuilder config;

        public RouteRegistry(SimpleResourceFactory resourceFactory, IHttpHostConfigurationBuilder config)
        {
            this.resourceFactory = resourceFactory;
            this.config = config;
        }

        public RouteRegistry MapServiceRoute<T>(string routePrefix, T service)
        {
            ServiceTypes["/" + routePrefix] = typeof(T);
            resourceFactory.Services[typeof(T)] = service;

//            RouteTable.Routes.MapServiceRoute<T>(routePrefix, config);

            return this;
        }

        public Option<KeyValuePair<string, Type>> GetServiceTypeForUri(Func<string, bool> predicate)
        {
            var matchingRoutes = ServiceTypes.Keys.Where(predicate).OrderBy(key => key.Length).ToList();

            if (matchingRoutes.Count > 0)
            {
                var matchingRoute = matchingRoutes.Last();

                return new KeyValuePair<string, Type>(matchingRoute, ServiceTypes[matchingRoute]).ToOption();
            }
            else
            {
                return new None<KeyValuePair<string, Type>>();
            }
        }

        public WebService DescribeService(string uri)
        {
            return new WebService
            {
                Uri = uri,
                ServiceType = ServiceTypes[uri].CodeString(),
                Methods = ServiceMethods(ServiceTypes[uri])
            };
        }

        public List<WebService> DescribeServices()
        {
            return ServiceTypes.Keys.Select(DescribeService).ToList();
        }

        private static List<WebMethod> ServiceMethods(Type serviceType)
        {
            return AllTypes(serviceType).SelectMany(type => type.GetMethods()).SelectMany(ServiceMethods).ToList();
        }

        private static IEnumerable<Type> AllTypes(Type parentType)
        {
            if (parentType == null)
            {
                yield break;
            }

            yield return parentType;

            foreach (var type in parentType.GetInterfaces().SelectMany(AllTypes))
            {
                yield return type;
            }

            foreach (var type in AllTypes(parentType.BaseType))
            {
                yield return type;
            }
        }

        private static IEnumerable<WebMethod> ServiceMethods(MethodInfo method)
        {
            if (method.HasAttribute<WebGetAttribute>())
            {
                yield return CreateWebMethod(method, method.Attribute<WebGetAttribute>().UriTemplate, "GET");
            }
            else if (method.HasAttribute<WebInvokeAttribute>())
            {
                yield return CreateWebMethod(method, method.Attribute<WebInvokeAttribute>().UriTemplate,
                    method.Attribute<WebInvokeAttribute>().Method);
            }
        }

        private static WebMethod CreateWebMethod(MethodInfo method, string uriTemplate, string verb)
        {
            return new WebMethod
            {
                Id = method.Name.GetHashCode(),
                Uri = uriTemplate,
                Verb = verb,
                Name = method.Name,
                ReturnType = method.ReturnType.CodeString(),
                Parameters = method.GetParameters().Select(paramter => new WebMethodParameter
                {
                    Name = paramter.Name,
                    ParameterType = paramter.ParameterType.CodeString(),
                    Binding = uriTemplate.Contains("{" + paramter.Name + "}") ? "Path" : "None"
                }).ToList(),
                Example = method.OptionalAttribute<ExampleAttribute>().Fold(() => "", example => example.Value)
            };
        }
    }
}