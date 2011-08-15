using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Linq;

namespace com.trafigura.services.util
{
    public class UriFormatExtensionMessageChannel : DelegatingChannel
    {
        private readonly IDictionary<string, MediaTypeWithQualityHeaderValue> extensionMappings = 
            new Dictionary<string, MediaTypeWithQualityHeaderValue>();

        public UriFormatExtensionMessageChannel(HttpMessageChannel handler) : base(handler)
        {
        }

        public UriFormatExtensionMessageChannel AddMapping(string extension, string mediaType)
        {
            extensionMappings[extension] = new MediaTypeWithQualityHeaderValue(mediaType);

            return this;
        }

        protected override System.Threading.Tasks.Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            var segments = request.RequestUri.Segments;
            var lastSegment = segments.LastOrDefault();
            MediaTypeWithQualityHeaderValue mediaType;
            var found = extensionMappings.TryGetValue(lastSegment, out mediaType);
            
            if (found)
            {
                var newUri = request.RequestUri.OriginalString.Replace("/" + lastSegment, "");
                request.RequestUri = new Uri(newUri, UriKind.Absolute);
                request.Headers.Accept.Clear();
                request.Headers.Accept.Add(mediaType);
            }
            return base.SendAsync(request, cancellationToken);
        }

        protected override HttpResponseMessage Send(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            throw new NotImplementedException();
        }
    }
}