using System;
using loopyxl;

namespace LoopyXL
{
    public interface Marshaller<TFrom, TTo>
    {
        TTo From(TFrom from);
        TFrom To(TTo to);
    }

    public class CastingMarshaller<TFrom, TTo> : Marshaller<TFrom, TTo>
    {
        public TTo From(TFrom from)
        {
            return (TTo) (object) from;
        }

        public TFrom To(TTo to)
        {
            return (TFrom)(object) to;
        }
    }

    public class TypedObjectMarshaller : Marshaller<object, TypedObject>
    {
        public TypedObject From(object parameter)
        {
            if (parameter is double)
            {
                return new TypedObject { type = TypedObject.Type.DOUBLE, doubleValue = (double)parameter };
            }
            if (parameter is string)
            {
                return new TypedObject { type = TypedObject.Type.STRING, stringValue = parameter as string };
            }
            if (parameter is ExcelDna.Integration.ExcelEmpty || parameter is ExcelDna.Integration.ExcelMissing)
            {
                return new TypedObject { type = TypedObject.Type.STRING, stringValue = "" };
            }

            throw new MarshallException("Parameter: {0}, has unexpected type: {1}", parameter, parameter.GetType());
        }

        public object To(TypedObject typedObject)
        {
            switch (typedObject.type)
            {
                case TypedObject.Type.DOUBLE: return typedObject.doubleValue;
                case TypedObject.Type.STRING: return typedObject.stringValue;
                default: throw new MarshallException("TypedObject: {0}, has unknown type: {1}", typedObject, typedObject.type);
            }
        }
    }
}