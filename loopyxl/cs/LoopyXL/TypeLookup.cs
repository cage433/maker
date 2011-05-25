using System;
using System.Collections.Generic;
using log4net;

namespace LoopyXL
{
    public static class TypeLookup
    {
        private static readonly ILog log = LogManager.GetLogger(typeof(TypeLookup));

        private static readonly IDictionary<string, Type> primativeTypes = new Dictionary<string, Type>
        {
            {"double", typeof(double)},
            {"string", typeof(string)},
            {"object", typeof(object)},

            {"double[]", typeof(double[])},
            {"string[]", typeof(string[])},
            {"object[]", typeof(object[])},

            {"double[][]", typeof(double[,])},
            {"string[][]", typeof(string[,])},
            {"object[][]", typeof(object[,])},
        };

        public static Type GetType(string typeName)
        {
            log.Info("Lookup: " + typeName);

            return primativeTypes.ContainsKey(typeName) 
                ? primativeTypes[typeName]
                : Type.GetType(typeName).AssertNotNull("No type matching: " + typeName);
        }
    }
}