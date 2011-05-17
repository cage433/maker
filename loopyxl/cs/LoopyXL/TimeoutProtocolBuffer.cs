using System;
using loopyxl;

namespace LoopyXL
{
    public class TimeoutProtocolBuffer : ProtocolBuffer
    {
        private readonly object @lock = new object();

        private readonly ProtocolBuffer buffer;
        private readonly WaitFor<Response> timeout;

        private bool connected;

        public TimeoutProtocolBuffer(ProtocolBuffer buffer, TimeSpan timeout)
        {
            this.buffer = buffer;
            this.timeout = new WaitFor<Response>(timeout);
            this.Connected = false;
        }

        public Response Call(Request request)
        {
            return Run(request).GetOrElse(new Response { type = request.type, status = Response.Status.TIMEOUT });
        }

        private IOption<Response> Run(Request request)
        {
            return Connected ? timeout.Run(() => buffer.Call(request)) : new None<Response>();
        }

        public bool Connected
        {
            get
            {
                lock (@lock)
                {
                    return connected;
                }
            }
            set
            {
                lock (@lock)
                {
                    connected = value;
                }
            }
        }
    }
}