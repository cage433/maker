using System.IO;
using System.Net;
using System.Net.Sockets;
using loopyxl;
using NUnit.Framework;

namespace LoopyXL.Test
{
    [TestFixture]
    public class ProtocolBufferTest
    {
        [Test, Ignore]
        public void ShouldBeAbleToSendMessagesQuickly()
        {
            var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);

            IPAddress[] hostAddresses = Dns.GetHostAddresses("172.20.14.67");

            //socket.Connect("localhost", 9876);
            //socket.Connect("172.20.14.67", 9876);
            socket.Connect(hostAddresses, 9876);

            var bufferedStream = new BufferedStream(new NetworkStream(socket));

            var protocolBuffer = new PrefixingProtocolBuffer(bufferedStream);

            for (; ; )
            {
                Response response = protocolBuffer.Call(CreateRequest());
            }
        }

        private Request CreateRequest()
        {
            var invocationRequest = new InvocationRequest { methodId = 123 };
            invocationRequest.parameters.Add(new InvocationValue() { type = InvocationValue.Type.STRING_VALUE, stringValue = "123" });

            return new Request {type = MessageType.INVOCATION, invocation = invocationRequest};
        }

        [Test]
        public void Something()
        {
            var invocationValue = new InvocationValue {type = InvocationValue.Type.DOUBLE_VALUE, doubleValue = 6.0};

            object result = ProtocolBufferMethodInvoker.MarshalResult(invocationValue);

            Assert.IsNotNull(result);
            Assert.AreEqual(6.0, result);
        }
    }
}