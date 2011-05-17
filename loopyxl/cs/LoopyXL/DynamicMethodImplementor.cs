using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using log4net;

namespace LoopyXL
{
    public class DynamicMethodImplementor : MethodImplementor
    {
        private readonly ILog log = LogManager.GetLogger(typeof (DynamicMethodImplementor));

        private readonly Type targetType = typeof (StaticMethodInvoker);

        public IOption<DynamicMethod> ImplementMethod(Method method)
        {
            try
            {
                return new Some<DynamicMethod>(ImplementMethodUnsafe(method));
            }
            catch (Exception e)
            {
                log.Error("Unable to create method: " + method.Name, e);

                return new None<DynamicMethod>();
            }
        }

        public DynamicMethod ImplementMethodUnsafe(Method method)
        {
            log.Info("Implementing method: " + method.Name + "(" + method.ParameterTypes.Concat(", ") + "): " + method.ReturnType);

            var dynamicMethod = new DynamicMethod(method.Name, method.ReturnType, method.ParameterTypes, targetType);

            var parameterInfos = dynamicMethod.GetParameters();

            var il = dynamicMethod.GetILGenerator();

            // Create array
            var parameters = il.DeclareLocal(typeof (object[]));

            Log(il, "Method Entry: " + method.Name);

            il.Emit(OpCodes.Ldc_I4, parameterInfos.Length);
            il.Emit(OpCodes.Newarr, typeof(Object));
            il.Emit(OpCodes.Stloc, parameters);

            // Store values in array
            for (int parameterIndex = 0; parameterIndex < parameterInfos.Length; parameterIndex++)
            {
                ParameterInfo parameter = parameterInfos[parameterIndex];

                il.Emit(OpCodes.Ldloc, parameters);
                il.Emit(OpCodes.Ldc_I4, parameterIndex);
                il.Emit(OpCodes.Ldarg, parameterIndex);

                Box(il, parameter.ParameterType);
                
                il.Emit(OpCodes.Stelem_Ref);
            }

            il.Emit(OpCodes.Ldc_I4, method.Id);
            il.Emit(OpCodes.Ldloc, parameters);
            Log(il, "Invoking: " + method.Name);
            il.Emit(OpCodes.Call, targetType.GetMethod("Invoke", new[] { typeof(int), typeof(object[]) }));
            UnBox(il, method.ReturnType);
            Echo(il, method.ReturnType);

            il.Emit(OpCodes.Ret);

            return dynamicMethod;
        }

        private void UnBox(ILGenerator il, Type type)
        {
            if (CanBox(type))
            {
                il.Emit(OpCodes.Unbox_Any, type);
            }
        }

        private void Box(ILGenerator il, Type type)
        {
            if (CanBox(type))
            {
                il.Emit(OpCodes.Box, type);    
            }
        }

        private void Log(ILGenerator il, string toLog)
        {
            il.Emit(OpCodes.Ldstr, toLog);
            il.Emit(OpCodes.Call, targetType.GetMethod("Log", new[] { typeof(string) }));
        }

        private void Echo(ILGenerator il, Type type)
        {
            try
            {
                MethodInfo methodInfo = targetType.GetMethod("Echo", new[] { type });
                if (methodInfo != null)
                    il.Emit(OpCodes.Call, methodInfo);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }

        private bool CanBox(Type type)
        {
            return type.IsPrimitive;
        }
    }
}