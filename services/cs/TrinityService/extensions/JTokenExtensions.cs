using System;
using System.Linq;

namespace Newtonsoft.Json.Linq
{
    public static class JTokenExtensions
    {
        public static JToken Select(this JToken jToken, params Func<JToken, JToken>[] fs)
        {
            return new RecursiveSelector(fs.Aggregate((l, r) => (token => r(l(token))))).Select(jToken);
        }

        private class RecursiveSelector
        {
            private readonly Func<JToken, JToken> f;

            public RecursiveSelector(Func<JToken, JToken> f)
            {
                this.f = f;
            }

            public JToken Select(JToken jToken)
            {
                if (jToken is JObject)
                {
                    return f(new JObject((jToken as JObject).Children().Select(Select)));
                }
                if (jToken is JArray)
                {
                    return f(new JArray((jToken as JArray).Children().Select(Select)));
                }
                if (jToken is JProperty)
                {
                    var prop = jToken as JProperty;

                    return f(new JProperty(prop.Name, Select(prop.Value)));
                }

                return f(jToken);
            }
        }
    }
}