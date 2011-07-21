using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;

namespace com.trafigura.services.util
{
    public class JsonSerializer
    {
        private readonly JsonSerializerSettings settings = new JsonSerializerSettings
        {
            Converters = new List<JsonConverter> { new StringEnumConverter() },
            TypeNameHandling = TypeNameHandling.All
        };

        public string Serialize(object value)
        {
            var jObject = JObject.Parse(JsonConvert.SerializeObject(value, Formatting.Indented, settings));

//            Console.WriteLine("Before conversion to EDM\n" + jObject);

            var jToken = jObject.Select(ConvertTypeToEDM);
//            Console.WriteLine("Before erasure\n" + jToken);

            var eraseCollections = EraseCollections(jToken);

//            Console.WriteLine("After erasure\n" + eraseCollections);

            return eraseCollections.ToString();
        }

        private static JToken ConvertTypeToEDM(JToken arg)
        {
            if (arg is JProperty)
            {
                var prop = arg as JProperty;
                if (prop.Name == "$type")
                {
                    var classPlusAssembly = prop.Value.ToString();

                    return new JProperty("Type", classPlusAssembly.Substring(1, classPlusAssembly.IndexOf(',') - 1));
                }
            }

            return arg;
        }

        private static JToken EraseCollections(JToken token)
        {
            if (token is JObject)
            {
                var jobject = token as JObject;

                if (jobject.PropertyValues().Any(IsList))
                {
                    return EraseCollections(jobject.Property("$values").Value);
                }
            }

            return token;
        }

        private static bool IsList(JToken propValue)
        {
            if (propValue is JValue)
            {
                var jvalue = propValue as JValue;

                return jvalue.Value.ToString().StartsWith("System.Collections.Generic.List");
            }

            return false;
        }
    }
}