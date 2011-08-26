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
        public readonly IDictionary<Type, object> Services = new Dictionary<Type, object>();

        public object GetInstance(Type serviceType, InstanceContext instanceContext, HttpRequestMessage request)
        {
            if (!Services.ContainsKey(serviceType))
            {
                throw new Exception(string.Format("\nService type not registered {0}, available services: \n\t{1}\n", serviceType.CodeString(),
                    Services.Keys.Select(type => type.CodeString()).Join("\n\t")));
            }

            return Services[serviceType];
        }

        public void ReleaseInstance(InstanceContext instanceContext, object service)
        {
        }
    }
}