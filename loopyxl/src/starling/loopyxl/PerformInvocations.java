package starling.loopyxl;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

public class PerformInvocations {
    public static void main(String[] args) throws Exception {
        Socket socket = new Socket("localhost", 9876);

        OutputStream outputStream = socket.getOutputStream();
        InputStream inputStream = socket.getInputStream();


        LoopyXL.InvocationRequest invocationRequest = LoopyXL.InvocationRequest.newBuilder()
                .setMethodId(123)
                .addParameters(createString("123"))
                .build();

        LoopyXL.Request request = LoopyXL.Request.newBuilder()
                .setId(123)
                .setType(LoopyXL.MessageType.INVOCATION)
                .setInvocation(invocationRequest)
                .build();

        for (;;)
        {
            request.writeDelimitedTo(outputStream);

            LoopyXL.Response response = LoopyXL.Response.parseDelimitedFrom(inputStream);
        }

        //socket.close();
    }

    private static LoopyXL.InvocationValue createString(String value) {
        return LoopyXL.InvocationValue.newBuilder()
                .setType(LoopyXL.InvocationValue.Type.STRING_VALUE)
                .setStringValue(value)
                .build();
    }
}
