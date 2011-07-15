using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace ContactManager_Advanced
{
    public class TypeFinder
    {
        private readonly Assembly[] assemblies = AppDomain.CurrentDomain.GetAssemblies();
        private readonly IDictionary<string, Type> typeDictionary = new Dictionary<string, Type>();

        public Type FindType(string typeName)
        {
            return typeDictionary.GetOrInitialise(typeName, SearchAssembliesForType);
        }

        private Type SearchAssembliesForType(string typeName)
        {
            if (typeName.StartsWith("System.Collections.Generic.List"))
            {
                var typeParameter = typeName.Substring("System.Collections.Generic.List`1[[".Length);

                return typeof (List<>).MakeGenericType(new[] { SearchAssembliesForType(typeParameter) });
            }

            return assemblies.Select(assembly => assembly.GetType(typeName)).Where(type => type != null)
                             .FirstOr(() => Throw(typeName));
        }

        private Type Throw(string typeName)
        {
            throw new Exception(String.Format("Could not find type: '{0}', in assemblies: {1}",
                typeName, assemblies.Select(assembly => assembly.FullName).Join(", ")));
        }
    }
}