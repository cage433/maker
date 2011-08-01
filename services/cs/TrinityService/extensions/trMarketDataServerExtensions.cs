using System;
using com.trafigura.services.trinity;

namespace MarketData2
{
    public static class trMarketDataServerExtensions
    {
        public static bool Transact<R>(this trMarketDataServer marketData, Curve<R> curve, Action action) where R : Rate
        {
            return Transact(marketData, curve.Profile, curve.Commodity, action);
        }

        public static bool Transact(this trMarketDataServer marketData, Profile profile, string commodity, Action action)
        {
            return Transact(marketData, profile, commodity, () => { action(); return true; });
        }

        public static T Transact<R, T>(this trMarketDataServer marketData, Curve<R> curve, Func<T> func) where R : Rate
        {
            return Transact(marketData, curve.Profile, curve.Commodity, func);
        }

        public static T Transact<T>(this trMarketDataServer marketData, Profile profile, string commodity, Func<T> func)
        {
            try
            {
                marketData.LockCommodityRates(profile.Id, commodity);

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