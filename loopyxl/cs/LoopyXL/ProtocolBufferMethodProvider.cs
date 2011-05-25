using System;
using System.Collections.Generic;
using System.Linq;
using log4net;
using loopyxl;

namespace LoopyXL
{
    public class ProtocolBufferMethodProvider : MethodProvider
    {
        private readonly ILog log = LogManager.GetLogger(typeof(ProtocolBufferMethodProvider));

        private readonly ProtocolBuffer protocolBuffer;

        public ProtocolBufferMethodProvider(ProtocolBuffer protocolBuffer)
        {
            this.protocolBuffer = protocolBuffer;
        }

        public IList<Method> GetMethods()
        {
            log.Info("GetMethods");

            try
            {
                var response = protocolBuffer.Call(new Request { type = MessageType.LOOKUP, lookup = new LookupRequest() });

                if (response.status == Response.Status.SUCCESS)
                {
                    List<Method> methods =
                        response.lookup.definitions.Select(definition => Method.Create(definition)).Flatten().ToList();

                    foreach (var method in methods)
                    {
                        log.Info("method: " + method.Name);
                    }

                    return methods;
                }
                else
                {
                    log.Error("Lookup Response: " + response.status);

                    return new List<Method>();
                }
            }
            catch (Exception e)
            {
                log.Error("Unable to get methods", e);

                throw new MarshallException(e, "Unable to get methods");
            }
        }
    }
}