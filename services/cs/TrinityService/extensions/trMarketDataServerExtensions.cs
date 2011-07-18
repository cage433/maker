using System;

namespace MarketData2
{
    public static class trMarketDataServerExtensions
    {
        public static bool Transact(this trMarketDataServer marketData, int profileId, string commodity, Action action)
        {
            return Transact(marketData, profileId, commodity, () => { action(); return true; });
        }

        public static T Transact<T>(this trMarketDataServer marketData, int profileId, string commodity, Func<T> func)
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