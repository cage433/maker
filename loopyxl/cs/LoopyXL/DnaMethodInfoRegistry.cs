using System.Collections.Generic;
using System.Reflection;
using ExcelDna.Integration;

namespace LoopyXL
{
    public class DnaMethodInfoRegistry : MethodInfoRegistry
    {
        public void RegisterMethods(params MethodInfo[] methods)
        {
            Integration.RegisterMethods(new List<MethodInfo>(methods));
        }
    }
}