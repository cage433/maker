using System;

namespace MarketData2
{
    public static class trMarketDataServerExtensions
    {
        public static void Lock(this trMarketDataServer marketData, int profileId, string commodity, Action action)
        {
            Lock(marketData, profileId, commodity, () => { action(); return false; });
        }

        public static T Lock<T>(this trMarketDataServer marketData, int profileId, string commodity, Func<T> func)
        {
            try
            {
                marketData.LockCommodityRates(profileId, commodity);

                var result = func();

                marketData.Commit();

                return result;
            }
            catch
            {
                marketData.Rollback();

                throw;
            }
        }
    }
}