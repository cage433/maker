using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Reflection;
using System.ServiceModel.Web;
using System.Text;
using System.Threading.Tasks;
using System.Web;
using log4net;

namespace com.trafigura.services.util
{
    public class DocumentationChannel : DelegatingChannel
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(DocumentationChannel));

        private readonly string path;
        private readonly RouteRegistry routeRegistry;

        public DocumentationChannel(string path, RouteRegistry routeRegistry, HttpMessageChannel handler)
            : base(handler)
        {
            this.path = path.StripSuffix("/");
            this.routeRegistry = routeRegistry;
        }

        protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            logger.Info("requestUri: " + request.RequestUri);

            var serviceTypeOpt = routeRegistry.GetServiceTypeForUri(key => request.RequestUri.PathAndQuery.EndsWith(key));

            return serviceTypeOpt.Fold(() =>
            {
                return BaseSendAsync(request, cancellationToken);
            }, 
            serviceType =>
            {
                return Task.Factory.StartNew(() =>
                {
                    var serviceMethods = AllTypes(serviceType.Value).SelectMany(type => type.GetMethods()).SelectMany(ServiceMethods);

                    var rendered = serviceMethods.SelectMany(method => method.Render)
                        .GroupBy(pair => pair.Key, pair => pair.Value, (key, values) => new KeyValuePair<string, string>(key, values.Join("\n\n")))
                        .ToDictionary();

                    var forms = rendered["json"];
                    var scala = string.Format(@"<pre class='scala' style='display: none;'>
@Path(""{0}"")
trait {1} {{
{2}
}}</pre>", serviceType.Key, serviceType.Value.Name, rendered["scala"]);

                    var cs = string.Format(@"<pre class='cs' style='display: none;'>
[ServiceContract]
public interface {0}
{{
{1}
}}</pre>
", serviceType.Value.Name, rendered["cs"]);

                    var content = string.Format(templateStart, path, serviceType.Value.Name) + forms + scala + cs + templateEnd;

                    return new HttpResponseMessage
                    {
                        Content = new StringContent(content, Encoding.UTF8, "text/html")
                    };
                });
            });
        }

        private static IEnumerable<ServiceMethod> ServiceMethods(MethodInfo method)
        {
            if (IsGet(method))
            {
                yield return new WebGetMethod(method);
            }
            else if (IsInvoke(method))
            {
                yield return new WebInvokeMethod(method);
            }
        }

        private Task<HttpResponseMessage> BaseSendAsync(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            return base.SendAsync(request, cancellationToken);
        }

        private string templateStart = @"
<!doctype html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>{1}</title>
    <link rel='stylesheet' type='text/css' href='{0}/jsonVisualization.css' />
	<script type='text/javascript' src='{0}/yuiloader-dom-event.js'></script>
	<script type='text/javascript' src='{0}/json-min.js'></script>
    <script type='text/javascript' src='{0}/jsonVisualization.js'></script>
    <script src='{0}/jquery-1.6.2.min.js'></script>
    <script src='{0}/displayJson.js'></script>
  </head>
  <body>
	<label><input type='radio' name='style' id='json2HTML' value='0' checked='checked' />HTML</label>
	<label><input type='radio' name='style' id='json2JSON' value='1' />JSON</label>
    <label><input type='radio' name='style' id='scala' value='2'/>Scala</label>
    <label><input type='radio' name='style' id='cs' value='3' />CS</label>";

        private string templateEnd = @"
  </body>
</html>";

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

        private static bool IsGet(MethodInfo method)
        {
            var customAttributes = method.GetCustomAttributes(typeof(WebGetAttribute), true).ToList();

            return customAttributes.Count() > 0;
        }

        private static bool IsInvoke(MethodInfo method)
        {
            var customAttributes = method.GetCustomAttributes(typeof(WebInvokeAttribute), true).ToList();

            return customAttributes.Count() > 0;
        }
    }
}