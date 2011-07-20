using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.ServiceModel.Web;
using TypeExtensions = System.TypeExtensions;

namespace com.trafigura.services.util
{
    public class WebGetMethod : ServiceMethod
    {
        private readonly MethodInfo method;
        private readonly WebGetAttribute attribute;
        private readonly string example;

        public WebGetMethod(MethodInfo method)
        {
            this.method = method;
            this.attribute = method.GetCustomAttributes(typeof(WebGetAttribute), false)[0] as WebGetAttribute;
            this.example = method.GetCustomAttributes(typeof (ExampleAttribute), false)
                .Select(attribute => attribute as ExampleAttribute)
                .Where(attribute => attribute != null)
                .Select(exampleAttribute => exampleAttribute.Value).FirstOr(() => "");
        }

        public IEnumerable<KeyValuePair<string, string>> Render
        {
            get
            {
                yield return Tag("json", string.Format("<form class='json' id='{0}' action='{1}' method='get'><input type='text' name='params' value='{2}'/></form>",
                                           method.GetHashCode(), ParameterlessUri(), example));

                yield return Tag("scala", string.Format(
@"  @Path(""{0}"")
  @GET @Produces(Array(""application/json""))
  {1}", attribute.UriTemplate, ScalaSignature.Replace("<", "[").Replace(">", "]")));

                yield return Tag("cs", string.Format(
@"    [OperationContract]
    [WebGet(UriTemplate = ""{0}"")]
    {1}", attribute.UriTemplate, CSSignature.Replace("<", "&lt;").Replace(">", "&gt;")));
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
                var parameterTypes = method.GetParameters().Select(parameter => parameter.ParameterType.CodeString(true));

                var methodName = method.Name.ToCharArray();
                methodName[0] = methodName[0].ToString().ToLower().ToCharArray()[0];

                var methodStart = string.Format("def {0}(", new String(methodName));
                var padding = "".PadLeft(methodStart.Length + 2, ' ');

                var parameters = parameterTypes.Zip(UriTemplateNames, (type, name) =>
                    string.Format(@"@PathParam(""{0}"") {0}: {1}", name, type)).Join(",\n" + padding);

                return methodStart + string.Format("{0}): {1}", parameters, method.ReturnType.CodeString(true));
            }
        }

        private IEnumerable<string> UriTemplateNames
        {
            get { return attribute.UriTemplate.DropUntil(c => c == '/' || c == '{').Split("/{}".ToCharArray()).ToList().Where(s => s.Length > 0); }
        }

        private string ParameterlessUri()
        {
            var uriTemplate = attribute.UriTemplate;

            return uriTemplate.IndexOf("{") < 0 ? uriTemplate : uriTemplate.Substring(0, uriTemplate.IndexOf("{")).StripSuffix("/");
        }
    }
}