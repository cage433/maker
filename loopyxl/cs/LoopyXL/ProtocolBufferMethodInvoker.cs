using System;
using System.Collections.Generic;
using System.Linq;
using log4net;
using loopyxl;

namespace LoopyXL
{
    public class ProtocolBufferMethodInvoker : MethodInvoker
    {
        private static readonly ILog log = LogManager.GetLogger(typeof(ProtocolBufferMethodInvoker));

        private readonly ProtocolBuffer protocolBuffer;
        private readonly Dictionary<int, Method> methods;

        private IDictionary<String, String> translatedErrors = new Dictionary<string, string>
        {
            {"Attempted to read past the end of the stream.", "Server Disconnected"}
        };

        public ProtocolBufferMethodInvoker(ProtocolBuffer protocolBuffer, MethodProvider methodProvider)
        {
            this.protocolBuffer = protocolBuffer;
            this.methods = methodProvider.GetMethods().ToDictionary(method => method.Id);
        }

        public object Invoke(int methodId, params object[] args)
        {
            log.Info("Invoking method: " + methodId);

            try
            {
                var response = protocolBuffer.Call(new Request { type = MessageType.INVOCATION, invocation = InvocationRequest(methodId, args) });

                if (response.status == Response.Status.SUCCESS)
                {
                    log.Info("Result: '" + response.invocation.result + "'");

                    object result = MarshalResult(response.invocation.result);

                    log.Info("Result: '" + result + "'");

                    return result;
                }
                else
                {
                    return "#" + response.status;
                }
            }
            catch (Exception e)
            {
                log.Error("Could not invoke", e);

                return "#" + translatedErrors.GetOrElse(e.Message, e.Message);
            }
        }

        private InvocationRequest InvocationRequest(int methodId, IEnumerable<object> args)
        {
            return new InvocationRequest {methodId = methodId}.Update(r => r.parameters.AddRange(MarshalledParameters(methodId, args)));
        }

        private IEnumerable<InvocationValue> MarshalledParameters(int methodId, IEnumerable<object> args)
        {
            return methods[methodId].ParameterTypes.Zip(args).Select((type, arg) => MarshalParameter(type, arg));
        }

        private InvocationValue MarshalParameter(Type type, object parameter)
        {
            if (type == typeof(double[,])) return new DoubleMatrixMarshaller().From((double[,])parameter);
            if (type == typeof(string[,])) return new StringMatrixMarshaller().From((string[,])parameter);
            if (type == typeof(object[,])) return new TypedMatrixMarshaller().From((object[,])parameter);

            if (type == typeof(double[])) return MarshalDoubleArray((double[])parameter);
            if (type == typeof(string[])) return MarshalStringArray((string[])parameter);
            if (type == typeof(object[])) return MarshalObjectArray((object[])parameter);

            if (type == typeof(double)) return MarshalDoubleValue((double)parameter);
            if (type == typeof(string)) return MarshalStringValue((string) parameter);
            if (type == typeof(object)) return MarshalObjectValue(parameter);

            log.Error("Can't handle parameter type: " + type.Name);

            throw new MarshallException("Can't handle parameter type: " + type.Name);
        }

        private static InvocationValue MarshalDoubleValue(double value)
        {
            return new InvocationValue {type = InvocationValue.Type.DOUBLE_VALUE, doubleValue = value};
        }

        private static InvocationValue MarshalDoubleArray(double[] array)
        {
            var result = new InvocationValue {type = InvocationValue.Type.DOUBLE_ARRAY};

            result.doubleArray.AddRange(array);

            return result;
        }


        private static InvocationValue MarshalStringValue(string value)
        {
            return new InvocationValue { type = InvocationValue.Type.STRING_VALUE, stringValue = value };
        }

        private static InvocationValue MarshalStringArray(string[] array)
        {
            var result = new InvocationValue { type = InvocationValue.Type.STRING_ARRAY };

            result.stringArray.AddRange(array);

            return result;
        }

        private static InvocationValue MarshalObjectArray(object[] array)
        {
            var marshaller = new TypedObjectMarshaller();

            var result = new InvocationValue { type = InvocationValue.Type.OBJECT_ARRAY };

            result.objectArray.AddRange(array.Select(elem => marshaller.From(elem)));

            return result;
        }

        private static InvocationValue MarshalObjectValue(object parameter)
        {
            var marshaller = new TypedObjectMarshaller();

            return new InvocationValue { type = InvocationValue.Type.OBJECT_VALUE, objectValue = marshaller.From(parameter) };
        }

        private static object MarshalObjectValue(TypedObject result)
        {
            if (result.type == TypedObject.Type.DOUBLE)
            {
                return result.doubleValue;
            }
            else
            {
                return result.stringValue;
            }
        }

        public static object MarshalResult(InvocationValue result)
        {
            switch(result.type)
            {
                case InvocationValue.Type.DOUBLE_VALUE:
                    return result.doubleValue;
                case InvocationValue.Type.DOUBLE_ARRAY:
                    return result.doubleArray.ToArray();
                case InvocationValue.Type.DOUBLE_MATRIX:
                    return new DoubleMatrixMarshaller().To(result);
                case InvocationValue.Type.STRING_VALUE:
                    return result.stringValue;
                case InvocationValue.Type.STRING_ARRAY:
                    return result.stringArray.ToArray();
                case InvocationValue.Type.STRING_MATRIX:
                    return new StringMatrixMarshaller().To(result);
                case InvocationValue.Type.OBJECT_MATRIX:
                    return new TypedMatrixMarshaller().To(result);
                case InvocationValue.Type.OBJECT_VALUE:
                    return MarshalObjectValue(result.objectValue);
                default:
                    throw new MarshallException("Cannot marshall result type: " + Enum.GetName(typeof(InvocationValue.Type), result.type));
            }
        }
    }
}