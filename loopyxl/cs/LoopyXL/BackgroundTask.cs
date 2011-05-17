using System;
using System.Threading;

namespace LoopyXL
{
    public class BackgroundTask
    {
        private readonly Action action;
        private volatile bool running = false;

        public BackgroundTask(Action action)
        {
            this.action = action;
        }

        public void Start()
        {
            if (!running)
            {
                ThreadPool.QueueUserWorkItem(delegate { SafelyPerform(action); running = false; }, null);
            }
        }

        private static void SafelyPerform(Action action)
        {
            try { action(); } catch { }
        }
    }
}