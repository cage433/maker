using System;

namespace com.trafigura.services.util
{
    public class ExampleAttribute : Attribute
    {
        public string Value { get; set; }

        public ExampleAttribute(string value)
        {
            Value = value;
        }
    }
}