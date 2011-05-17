using System;
using System.IO;
using System.Threading;
using log4net;

namespace LoopyXL
{
    public class ReconnectingStream : Stream
    {
        public event Action Disconnected;

        private readonly ILog log = LogManager.GetLogger(typeof(ReconnectingStream));

        private readonly object @lock = new object();
        private readonly Func<IVolatile<Stream>> source;
        private readonly BackgroundTask reconnectionTask;
        private bool shouldReconnect;
        private readonly Action reconnectionAction;

        private IVolatile<Stream> volatileStream;

        public ReconnectingStream(Func<IVolatile<Stream>> source, bool shouldReconnect, Action reconnectionAction)
        {
            this.source = source;
            this.shouldReconnect = shouldReconnect;
            this.reconnectionAction = reconnectionAction;
            this.reconnectionTask = CreateReconnectionTask();
        }

        public void Connect()
        {
            shouldReconnect = true;
            ConnectStream();
        }

        public void Close()
        {
            try
            {
                shouldReconnect = false;

                Guard(stream => stream.Close());
            }
            catch
            {
            }
        }

        public override void Flush()
        {
            Guard(stream => stream.Flush());
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            return Guard(stream => stream.Seek(offset, origin));
        }

        public override void SetLength(long value)
        {
            Guard(stream => stream.SetLength(value));
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            return Guard(stream => stream.Read(buffer, offset, count));
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            Guard(stream => stream.Write(buffer, offset, count));
        }

        public override bool CanRead
        {
            get { return Guard(stream => stream.CanRead); }
        }

        public override bool CanSeek
        {
            get { return Guard(stream => stream.CanSeek); }
        }

        public override bool CanWrite
        {
            get { return Guard(stream => stream.CanWrite); }
        }

        public override long Length
        {
            get { return Guard(stream => stream.Length); }
        }

        public override long Position
        {
            get { return Guard(stream => stream.Position); }
            set { Guard(stream => stream.Position = value); }
        }

        private Stream Stream
        {
            get
            {
                lock (@lock)
                {
                    return volatileStream == null ? null : volatileStream.Value;
                }
            }
        }

        private void Guard(Action<Stream> action)
        {
            try
            {
                action(Stream.AssertNotNull("No Stream"));
            }
            catch
            {
                Invalidate();

                throw;
            }
        }

        private T Guard<T>(Func<Stream, T> func)
        {
            try
            {
                return func(Stream.AssertNotNull("No Stream"));
            }
            catch
            {
                Invalidate();

                throw;
            }
        }

        private void Invalidate()
        {
            Disconnected();

            if (volatileStream != null)
            {
                volatileStream.Invalidated -= Invalidate;

                try
                {
                    volatileStream.Value.Close();
                }
                catch
                {
                }

                volatileStream = null;
            }

            if (shouldReconnect)
            {
                reconnectionTask.Start();
            }
        }

        private BackgroundTask CreateReconnectionTask()
        {
            return new BackgroundTask(() =>
            {
                if (shouldReconnect)
                {
                    log.Info("Reconnection task started");

                    while (shouldReconnect && Stream == null)
                    {
                        lock (@lock)
                        {
                            ConnectStream();
                        }

                        Thread.Sleep(500);
                    }

                    reconnectionAction();

                    log.Info("Reconnection task done");
                }
            });
        }

        private IVolatile<Stream> ConnectStream()
        {
            try
            {
                IVolatile<Stream> volatileStream = this.volatileStream = source();

                volatileStream.Invalidated += Invalidate;

                return volatileStream;
            }
            catch (Exception e)
            {
                return null;
            }
        }
    }
}