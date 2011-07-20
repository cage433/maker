using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.ServiceModel.Web;

namespace com.trafigura.services.util
{
    public class WebInvokeMethod : ServiceMethod
    {
        private MethodInfo method;
        private WebInvokeAttribute attribute;

        public WebInvokeMethod(MethodInfo method)
        {
            this.method = method;
            this.attribute = method.GetCustomAttributes(typeof(WebInvokeAttribute), false)[0] as WebInvokeAttribute;
        }

        public IEnumerable<KeyValuePair<string, string>> Render
        {
            get
            {
                yield return Tag("scala", string.Format(
@"  @Path(""{0}"")
  @{1} @Consumes(Array(""application/json"")) @Produces(Array(""application/json""))
  {2}", attribute.UriTemplate, attribute.Method, ScalaSignature.Replace("<", "[").Replace(">", "]")));

                yield return Tag("cs", string.Format(
@"    [OperationContract]
    [WebInvoke(UriTemplate = ""{0}"", Method=""{1}"")]
    {2}", attribute.UriTemplate, attribute.Method, CSSignature.Replace("<", "&lt;").Replace(">", "&gt;")));
            }
        }

        private static KeyValuePair<string, string> Tag(string key, string value)
        {
            return new KeyValuePair<string, string>(key, value);
        }

        private string CSSignature
        {
            get
            {
                var parameters = method.GetParameters().Select(parameter => parameter.ParameterType.CodeString(true));

                return string.Format("{0} {1}({2});", method.ReturnType.CodeString(true), method.Name,
                                     parameters.Zip(UriTemplateNames, (type, name) => type + " " + name).Join(", "));
            }
        }

        private string ScalaSignature
        {
            get
            {
                var parameterTypes = method.GetParameters().Select(parameter => parameter.ParameterType.CodeString(true)).ToList();

                var methodStart = string.Format("def {0}(", method.Name.Decapitalize());
                var padding = "".PadLeft(methodStart.Length + 2, ' ');

                var pathParameters = parameterTypes.Zip(UriTemplateNames, (type, name) =>
                    string.Format(@"@PathParam(""{0}"") {0}: {1}", name, type)).ToList();

                var nonPathParameters = GetNonPathParameters(parameterTypes, UriTemplateNames);

                var parameters = (pathParameters.Concat(nonPathParameters)).Join(",\n" + padding);

                return methodStart + string.Format("{0}): {1}", parameters, method.ReturnType.CodeString(true));
            }
        }

        private static IEnumerable<string> GetNonPathParameters(List<string> parameterTypes, List<string> uriTemplateNames)
        {
            if (parameterTypes.Count == uriTemplateNames.Count) return new List<string>();
            else
            {
                return parameterTypes.GetRange(uriTemplateNames.Count, parameterTypes.Count - uriTemplateNames.Count)
                    .Select(type => type.Decapitalize().Replace("<", "").Replace(">", "") + ": " + type);
            }
        }

        private List<string> UriTemplateNames
        {
            get { return attribute.UriTemplate.DropUntil(c => c == '/' || c == '{').Split("/{}".ToCharArray()).ToList().Where(s => s.Length > 0).ToList(); }
        }

        private string ParameterlessUri()
        {
            var uriTemplate = attribute.UriTemplate;

            return uriTemplate.IndexOf("{") < 0 ? uriTemplate : uriTemplate.Substring(0, uriTemplate.IndexOf("{")).StripSuffix("/");
        }
    }
}