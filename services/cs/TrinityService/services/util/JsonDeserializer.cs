using System;
using System.Collections.Generic;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;

namespace com.trafigura.services.util
{
    public class JsonDeserializer
    {
        private readonly JsonSerializerSettings settings = new JsonSerializerSettings
        {
            Converters = new List<JsonConverter> { new StringEnumConverter() },
            TypeNameHandling = TypeNameHandling.All
        };

        private readonly TypeFinder typeFinder = new TypeFinder();

        public object Deserialize(string edmJson, Type type)
        {
            var dotNetJson = edmJson.ParseJson().Select(ConvertTypeToNet).ToString();

//            Console.WriteLine("After conversion to .net\n" + dotNetJson);

            return JsonConvert.DeserializeObject(dotNetJson, type, settings);
        }

        public string Pretty(string json)
        {
            return JObject.Parse(json).ToString(Formatting.Indented);
        }

        private JToken ConvertTypeToNet(JToken arg)
        {
            if (arg is JProperty)
            {
                var prop = arg as JProperty;
                if (prop.Name == "Type")
                {
                    return new JProperty("$type", new JValue(GetFullyQualifiedTypeName((JValue)prop.Value)));
                }
            }

            return arg;
        }

        private string GetFullyQualifiedTypeName(JValue typeName)
        {
            var fullyQualifiedTypeName = typeFinder.FindType(typeName.Value.ToString()).AssemblyQualifiedName;

            return fullyQualifiedTypeName;
        }
    }
}