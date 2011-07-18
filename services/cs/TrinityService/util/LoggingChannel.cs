namespace ContactManager_Advanced
{
    using System.Net.Http;

    public class LoggingChannel : DelegatingChannel
    {
        public LoggingChannel(HttpMessageChannel handler)
            :base(handler)
        {
            
        }

        protected override System.Threading.Tasks.Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            System.Diagnostics.Trace.TraceInformation("Begin Request: {0} {1}", request.Method, request.RequestUri);
            return base.SendAsync(request, cancellationToken);
        }
    }
}