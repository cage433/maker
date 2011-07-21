using System;
using com.trafigura.services.trinity;

namespace MarketData2
{
    public static class trMarketDataServerExtensions
    {
        public static bool Transact<R>(this trMarketDataServer marketData, Curve<R> curve, Action action) where R : Rate
        {
            return Transact(marketData, curve.ProfileId, curve.Commodity, action);
        }

        public static bool Transact(this trMarketDataServer marketData, int profileId, string commodity, Action action)
        {
            return Transact(marketData, profileId, commodity, () => { action(); return true; });
        }

        public static T Transact<R, T>(this trMarketDataServer marketData, Curve<R> curve, Func<T> func) where R : Rate
        {
            return Transact(marketData, curve.ProfileId, curve.Commodity, func);
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