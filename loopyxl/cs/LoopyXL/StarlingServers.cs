using System;
using System.Collections.Generic;
using ExcelDna.Integration;
using log4net;
using log4net.Config;
using log4net.Core;

namespace LoopyXL
{
    public class StarlingServers
    {
        private static StarlingServers Instance;

        private readonly ILog log = LogManager.GetLogger(typeof(StarlingServers));
        private readonly IList<StarlingServer> servers = new List<StarlingServer>();

        static StarlingServers()
        {
            LogManager.GetRepository().Threshold = Level.Off;
            BasicConfigurator.Configure(new ExcelDNAAppender());
        }

        public StarlingServers(Level loggingLevel)
        {
            LogManager.GetRepository().Threshold = loggingLevel;

            Instance = this;
        }

        public StarlingServers Add(String host, int port)
        {
            if (servers.Count == 6)
            {
                log.Error("Cannot register more than 6 servers");
                throw new Exception("Cannot register more than 6 servers");
            }

            var server = new StarlingServer(host, port);

            servers.Add(server);

            log.Info("Adding server: " + host + ": " + port);

            XlCall.Excel(XlCall.xlfAddCommand, 1, 1, (object)new object[] { "Connect to: " + server, "Connect" + (servers.Count - 1) }, 1);

            return this;
        }

        // Don't know how to dynamically create commands, or to send a payload with a command, so hard-code method names to server indicies.
        [ExcelCommand] public static void Connect0() { Instance.ConnectTo(0); }
        [ExcelCommand] public static void Connect1() { Instance.ConnectTo(1); }
        [ExcelCommand] public static void Connect2() { Instance.ConnectTo(2); }
        [ExcelCommand] public static void Connect3() { Instance.ConnectTo(3); }
        [ExcelCommand] public static void Connect4() { Instance.ConnectTo(4); }
        [ExcelCommand] public static void Connect5() { Instance.ConnectTo(5); }

        public void ConnectTo(int serverIndex)
        {
            foreach (var server in servers)
            {
                server.Disconnect();
            }

            servers[serverIndex].Connect();
        }
    }
}