using System.IO;
using log4net;
using loopyxl;
using ProtoBuf;

namespace LoopyXL
{
    public class PrefixingProtocolBuffer : ProtocolBuffer
    {
        private readonly ILog log = LogManager.GetLogger(typeof(PrefixingProtocolBuffer));

        private readonly Stream stream;
        private int id = 1;

        public PrefixingProtocolBuffer(Stream stream)
        {
            this.stream = stream;
        }

        public Response Call(Request request)
        {
            Send(request);

            var response = Retrieve();

            if (response.requestId != request.id)
            {
                throw new MarshallException("Response does not match request");
            }

            if (response.type != request.type)
            {
                throw new MarshallException("Response type does match request type");
            }

            return response;
        }

        private void Send(Request request)
        {
            request.id = id++;

            log.Info("Sending " + request.type);

            Serializer.SerializeWithLengthPrefix(stream, request, PrefixStyle.Base128);
        }

        private Response Retrieve()
        {
            var response = Serializer.DeserializeWithLengthPrefix<Response>(stream, PrefixStyle.Base128);

            log.Info("Retrieved " + response.type + ", status: " + response.status);

            return response;
        }
    }
}