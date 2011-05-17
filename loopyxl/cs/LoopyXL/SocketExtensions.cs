using log4net;

namespace System.Net.Sockets
{
    public static class SocketExtensions
    {
        private static readonly ILog log = LogManager.GetLogger(typeof(SocketExtensions));

        public static bool IsConnected(this Socket socket)
        {
            bool isConnected = CheckConnected(socket);

            if (!isConnected)
            {
                log.Info("Socket Disconnected");
            }

            return isConnected;
        }

        private static bool CheckConnected(Socket socket)
        {
            try
            {
                return !(socket.Poll(1, SelectMode.SelectRead) && socket.Available == 0);
            }
            catch (SocketException)
            {
                return false;
            }
        }
    }
}