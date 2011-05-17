using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Reflection.Emit;
using System.Security.Principal;
using log4net;
using loopyxl;

namespace LoopyXL
{
    public class StarlingServer
    {
        private readonly ILog log = LogManager.GetLogger(typeof(StarlingServer));

        private readonly string host;
        private readonly int port;
        private readonly MethodImplementor methodImplementor;
        private readonly MethodInfoRegistry methodInfoRegistry;
        private readonly ReconnectingStream reconnectingStream;
        private readonly TimeoutProtocolBuffer protocolBuffer;
        private readonly MethodProvider methodProvider;

        public StarlingServer(String host, int port)
        {
            this.host = host;
            this.port = port;
            this.methodImplementor = new DynamicMethodImplementor();
            this.methodInfoRegistry = new DnaMethodInfoRegistry();
            this.reconnectingStream = new ReconnectingStream(GetStream, false, AuthenticateAndLookup);
            this.protocolBuffer = new TimeoutProtocolBuffer(new PrefixingProtocolBuffer(reconnectingStream), new TimeSpan(0, 0, 30));
            this.methodProvider = new ProtocolBufferMethodProvider(protocolBuffer).Cache();

            reconnectingStream.Disconnected += () => protocolBuffer.Connected = false;
        }

        public override string ToString()
        {
            return host + ":" + port;
        }

        public void Connect()
        {
            reconnectingStream.Connect();
            AuthenticateAndLookup();
        }

        private void Authenticate()
        {
            var response = protocolBuffer.Call(new Request { type = MessageType.AUTHENTICATE, authenticate = AuthenticateRequest() });

            if (response.status == Response.Status.FAILURE)
            {
                log.Error("Could not authenticate");
            }
        }

        private AuthenticateRequest AuthenticateRequest()
        {
            return new AuthenticateRequest { ticket = KerberosToken().GetRequest()};
        }

        private KerberosRequestorSecurityToken KerberosToken()
        {
            return new KerberosRequestorSecurityToken(
                "STARLING-TEST/dave-linux",
                TokenImpersonationLevel.Identification,
                CredentialCache.DefaultNetworkCredentials, new Guid().ToString());
        }

        public void Disconnect()
        {
            reconnectingStream.Close();
        }

        private IVolatile<Stream> GetStream()
        {
            log.Info(string.Format("Connecting to: {0}...", this));

            var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            socket.Connect(host, port);

            log.Info("Socket Connected...");

            protocolBuffer.Connected = true;

            log.Info("protocolBuffer.Connected = " + protocolBuffer.Connected);


            return new Volatile<Stream>(new BufferedStream(new NetworkStream(socket)), socket.IsConnected);
        }

        public void AuthenticateAndLookup()
        {
            Authenticate();

            StaticMethodInvoker.MethodInvoker = new ProtocolBufferMethodInvoker(protocolBuffer, methodProvider);

            var implementations = methodProvider.GetMethods().Select(method => methodImplementor.ImplementMethod(method)).Flatten();

            methodInfoRegistry.RegisterMethods(implementations.ToArray());
        }
    }
}