using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http.Headers;
using Microsoft.ApplicationServer.Http;
using Newtonsoft.Json;

namespace com.trafigura.services.util
{
    public class JsonFormatter : MediaTypeFormatter
    {
        private readonly JsonDeserializer deserializer = new JsonDeserializer();
        private readonly JsonSerializer serializer = new JsonSerializer(TypeNameHandling.All);
        private readonly IDictionary<Type, Func<string, object>> deserializers = new Dictionary<Type, Func<string, object>>
        {
            {typeof(bool), text => bool.Parse(text)},
            {typeof(int),  text => int.Parse(text)}
        };

        public JsonFormatter()
        {
            SupportedMediaTypes.Add(new MediaTypeHeaderValue("application/json"));
        }

        public override object OnReadFromStream(Type type, Stream stream, HttpContentHeaders contentHeaders)
        {
            var text = new StreamReader(stream).ReadToEnd();

            var primativeOrJsonDeserializer = deserializers.GetOrElse(type, (ignore) => deserializer.Deserialize(text, type));

            return primativeOrJsonDeserializer(text);
        }

        public override void OnWriteToStream(Type type, object value, Stream stream, HttpContentHeaders contentHeaders, 
            TransportContext context)
        {
            using (var streamWriter = new StreamWriter(stream) {AutoFlush = true})
            {
                var text = type.IsPrimitive || type == typeof(string) ? value.ToString() : serializer.Serialize(value);

                streamWriter.Write(text);
            }
        }

        protected override bool OnCanReadType(Type type)
        {
            return true;
        }

        protected override bool OnCanWriteType(Type type)
        {
            return true;
        }
    }
}