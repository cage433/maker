using System;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;

namespace com.trafigura.services.util
{
    public class JsonSerializer
    {
        private readonly JsonSerializerSettings settings;

        public JsonSerializer(TypeNameHandling typeNameHandling = TypeNameHandling.All)
        {
            settings = new JsonSerializerSettings
            {
                Converters = new List<JsonConverter> { new StringEnumConverter() },
                TypeNameHandling = TypeNameHandling.Objects
            };
        }

        public string Serialize(object value)
        {
            var serializeObject = JsonConvert.SerializeObject(value, Formatting.Indented, settings);

            var jObject = serializeObject.ParseJson();

//            Console.WriteLine("Before conversion to EDM\n" + jObject);

            var jToken = jObject.Select(ConvertTypeToEDM);
//            Console.WriteLine("Before erasure\n" + jToken);

            var eraseCollections = jToken.Select(EraseCollections);

//            Console.WriteLine("After erasure\n" + eraseCollections);)

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

                var propertyValues = jobject.PropertyValues();
                if (propertyValues.Any(IsList))
                {
                    var jProperty = jobject.Property("$values");

                    if (jProperty != null)
                    {
                        return EraseCollections(jProperty.Value);
                    }
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