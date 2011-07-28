using System.Collections.Generic;

namespace com.trafigura.services.meta
{
    public class WebService
    {
        public string Uri;
        public string ServiceType;
        public List<WebMethod> Methods;
    }

    public class WebMethod
    {
        public int Id;
        public string Uri;
        public string Verb;
        public string Name;
        public string ReturnType;
        public List<WebMethodParameter> Parameters;
        public string Example = "";
    }

    public class WebMethodParameter
    {
        public string Name;
        public string ParameterType;
        public string Binding;
    }
}