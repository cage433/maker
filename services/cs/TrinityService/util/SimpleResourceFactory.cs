using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.ServiceModel;
using Microsoft.ApplicationServer.Http.Description;

namespace com.trafigura.services.util
{
    public class SimpleResourceFactory : IResourceFactory
    {
        private readonly IDictionary<Type, object> resources;

        public SimpleResourceFactory(params object[] resources)
            : this(resources.ToDictionary(resource => resource.GetType()))
        {
        }

        public SimpleResourceFactory(IDictionary<Type, object> resources)
        {
            this.resources = resources;
        }

        public object GetInstance(Type serviceType, InstanceContext instanceContext, HttpRequestMessage request)
        {
            if (!resources.ContainsKey(serviceType))
            {
                throw new Exception(string.Format("Service type not registered {0}, available services: [{1}]", serviceType.Name,
                    resources.Keys.Select(type => type.Name).Join(", ")));
            }

            return resources[serviceType];
        }

        public void ReleaseInstance(InstanceContext instanceContext, object service)
        {
        }
    }
}