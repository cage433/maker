using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.IO;
using System.Net.Http;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.ServiceModel.Web;
using com.trafigura.services.meta;

namespace com.trafigura.services.util
{
    [Export]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class DocumentationService : DocumentationServiceApi
    {
        private readonly IDictionary<string, string> extensionMediaTypes = new Dictionary<string, string>
        {
            {".html", "text/html"},
            {".htm", "text/html"},
            {".js", "text/javascript"},
            {".css", "text/css"},
            {".gif", "image/gif"}
        };

        private readonly RouteRegistry routeRegistry;

        public DocumentationService(RouteRegistry routeRegistry)
        {
            this.routeRegistry = routeRegistry;
        }

        public HttpResponseMessage File(string filename)
        {
            Stream stream = new FileStream(filename, FileMode.Open);

            //Set the correct context type for the file requested.
            int extIndex = filename.LastIndexOf(".");
            string extension = filename.Substring(extIndex, filename.Length - extIndex);

            var mediaType = extensionMediaTypes.GetOrElse(extension, () =>
            {
                throw new ApplicationException("File type not supported");
            });

            WebOperationContext.Current.OutgoingResponse.ContentType = mediaType;

            return new HttpResponseMessage
            {
                Content = new StreamContent(stream)
            };
        }

        public WebService MetaData(string path)
        {
            return routeRegistry.DescribeService("/" + path);
        }

        public List<WebService> AllMetaData()
        {
            return routeRegistry.DescribeServices();
        }
    }
}