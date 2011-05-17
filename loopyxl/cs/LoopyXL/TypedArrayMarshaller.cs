using System;
using System.Linq;
using loopyxl;

namespace LoopyXL
{
    public class TypedArrayMarshaller
    {
        private readonly TypedObjectMarshaller marshaller = new TypedObjectMarshaller();

        public InvocationValue From(object[] parameters)
        {
            // TODO: Return an empty 'objectArray' when it only contains 'ExcelMissing' or 'ExcelEmpty'
            return new InvocationValue {type = InvocationValue.Type.OBJECT_ARRAY}
                .Update(r => r.objectArray.AddRange(parameters.Select(parameter => marshaller.From(parameter))));
        }

        public object[] To(InvocationValue value)
        {
            return value.objectArray.Select(typedObject => marshaller.To(typedObject)).ToArray();
        }
    }
}