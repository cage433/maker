using System;
using System.Collections.Generic;
using System.Json;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Reflection;
using System.ServiceModel.Web;
using System.Text;
using System.Threading.Tasks;
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

            var serviceTypeOpt = routeRegistry.GetServiceTypeForUri(key => request.RequestUri.PathAndQuery.Equals(key));

            return serviceTypeOpt.Fold(() =>
            {
                return BaseSendAsync(request, cancellationToken);
            }, 
            serviceType =>
            {
                return Task.Factory.StartNew(() => GenerateResponse(serviceType.Value, serviceType.Key, request.Headers));
            });
        }

        private HttpResponseMessage GenerateResponse(Type serviceType, string binding, HttpRequestHeaders headers)
        {
            var content = string.Format(templateHead, path, serviceType.Name)
                + templateBodyStart + string.Format(templateBodyEnd, binding);

            return new HttpResponseMessage
            {
                Content = new StringContent(content, Encoding.UTF8, "text/html")
            };
        }

        private Task<HttpResponseMessage> BaseSendAsync(HttpRequestMessage request, System.Threading.CancellationToken cancellationToken)
        {
            return base.SendAsync(request, cancellationToken);
        }

        private string templateHead =
            @"<!doctype html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>{1}</title>
    <link rel='stylesheet' type='text/css' href='{0}Doc/Files/jsonVisualization.css' />
	<script type='text/javascript' src='{0}Doc/Files/yuiloader-dom-event.js'></script>
	<script type='text/javascript' src='{0}Doc/Files/json-min.js'></script>
    <script type='text/javascript' src='{0}Doc/Files/jsonVisualization.js'></script>
    <script type='text/javascript' src='{0}Doc/Files/jquery-1.6.2.min.js'></script>
    <script type='text/javascript' src='{0}Doc/Files/jquery.tmpl.js'></script>
    <script type='text/javascript' src='{0}Doc/Files/auto-grow.js'></script>
    <script type='text/javascript' src='{0}Doc/Files/displayJson.js'></script>
  </head>";

        private string templateBodyStart =
            @"   <body>
    <script id='scalaTemplate' type='text/x-jquery-tmpl'><pre>${ServiceType.packageClause()}<br/><br/>${imports(ServiceType, Methods)}@Path(""${Uri}"")<br/>trait ${ServiceType.packageless()} extends DocumentedService {<br/>{{tmpl(Methods) '#scalaMethod'}}}<br/></pre></script>

    <script id='scalaMethod' type='text/x-jquery-tmpl'>
&nbsp;&nbsp;@Path(""${Uri}"")<br/>
&nbsp;@${Verb}{{if Verb == 'POST' || Verb == 'PUT'}} @Consumes(Array(""application/json"")){{/if}} @Produces(Array(""application/json""))<br/>
&nbsp;def ${Name.decapitalize()}({{each(i, param) Parameters}}${renderer.scalaParameter(i, param, Parameters.length)}{{/each}}): ${ReturnType.scalaType()}<br/>
<br/>
    </script>

    <script id='csTemplate' type='text/x-jquery-tmpl'><pre>${usings(ServiceType, Methods)}${ServiceType.namespaceStart()}&nbsp;&nbsp;&nbsp;&nbsp;[ServiceContract]<br/>&nbsp;&nbsp;&nbsp;&nbsp;public interface ${ServiceType.packageless()}<br/>&nbsp;&nbsp;&nbsp;&nbsp;{<br/>{{tmpl(Methods) '#csMethod'}}&nbsp;&nbsp;&nbsp;&nbsp;}<br/>${ServiceType.namespaceEnd()}</pre></script>

    <script id='csMethod' type='text/x-jquery-tmpl'>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[OperationContract]<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${renderer.csMethod(Uri, Verb)}<br/>{{if Example != ''}}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[Example(""${Example}"")]<br/>{{/if}}
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;${ReturnType.packageless()} ${Name}({{each(i, param) Parameters}}${renderer.csParameter(i, param, Parameters.length)}{{/each}});<br/>
<br/>
    </script>

    <script id='formsTemplate' type='text/x-jquery-tmpl'>
        {{tmpl(Methods) '#formTemplate'}}
    </script>

    <script id='formTemplate' type='text/x-jquery-tmpl'>
        {{if Verb == 'GET'}}
            <form id='${Id}' class='json' action='${renderer.parameterlessUri(Uri, Parameters.length)}' method='${Verb}'><input type='text' value='${Example}'/></form>
        {{/if}}
    </script>

	<label><input type='radio' name='renderingStyle' id='html' value='json2HTML' checked='checked' />HTML</label>
	<label><input type='radio' name='renderingStyle' id='json' value='json2JSON' />JSON</label>
    <label><input type='radio' name='renderingStyle' id='scala' value='showScala'/>Scala</label>
    <label><input type='radio' name='renderingStyle' id='cs'    value='showCS' />CS</label>";

        private string templateBodyEnd =
@"     <div id='service' href='{0}'>
        <div class='json html rendering'></div>
        <div class='scala rendering'></div>
        <div class='cs rendering'></div>
    </div>
  </body>
</html>";

        
    }
}