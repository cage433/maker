using System.Collections.Generic;

namespace com.trafigura.services.util
{
    public interface ServiceMethod
    {
        IEnumerable<KeyValuePair<string, string>> Render { get; }
    }
}