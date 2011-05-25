using System;
using System.Collections.Generic;
using System.Linq;
using log4net;

namespace LoopyXL
{
    public class CachingMethodProvider : MethodProvider
    {
        private readonly ILog log = LogManager.GetLogger(typeof(CachingMethodProvider));
        private readonly object @lock = new object();

        private readonly MethodProvider methodProvider;
        private IList<Method> methods;
        private Dictionary<int, Method> methodsDictionary;

        public CachingMethodProvider(MethodProvider methodProvider)
        {
            this.methodProvider = methodProvider;
        }

        private IList<Method> Methods
        {
            get { return methods; }
            set 
            { 
                methods = value;
                methodsDictionary = methods.ToDictionary(method => method.Id);
            }
        }

        public IList<Method> GetMethods()
        {
            lock (@lock)
            {
                if (Methods == null)
                {
                    Methods = methodProvider.GetMethods();
                }

                return Methods;
            }
        }

        public Method this[int methodId]
        {
            get
            {
                if (methodsDictionary == null)
                {
                    throw new MarshallException("No methods available");
                }

                if (!methodsDictionary.ContainsKey(methodId))
                {
                    throw new MarshallException("No matching method for id: " + methodId);
                }

                Method method = methodsDictionary[methodId];

                log.Info("Looking up: " + methodId + " = " + method.Name);

                return method;
            }
        }
    }
}