using System;
using System.Threading;
using NUnit.Framework;

namespace LoopyXL.Test
{
    [TestFixture]
    public class WaitForTest
    {
        [Test]
        public void ShouldTimeout()
        {
            Assert.AreEqual(999, SleepFor(1200, 123).GetOrElse(999));
        }

        [Test]
        public void ShouldNotTimeout()
        {
            Assert.AreEqual(123, SleepFor(800, 123).GetOrElse(999));
        }

        private IOption<int> SleepFor(int timeout, int result)
        {
            return new WaitFor<int>(new TimeSpan(0, 0, 1)).Run(() =>
            {
                Thread.Sleep(timeout);

                return result;
            });
        }
    }
}