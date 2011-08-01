using System;
using System.Collections.Generic;
using System.Linq;
using log4net;
using MarketData2;

namespace com.trafigura.services.trinity
{
    public abstract class Curve<R> where R : Rate
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(Curve<R>));

        protected readonly trMarketDataServer marketData;

        public Profile Profile { get; private set; }
        public string Commodity { get; private set; }

        protected Curve(trMarketDataServer marketData, Profile profile, string commodity)
        {
            this.marketData = marketData;
            Profile = profile;
            Commodity = commodity;
        }

        protected abstract trRateCurve TrinityRateCurve { get; }
        protected abstract trRateCurve GetOrCreateTrinityRateCurve();

        public abstract List<R> Rates { get; }

        public bool SetRates(List<R> rates)
        {
            return AddOrSetRates(rates, true);
        }
       
        public bool AddRates(List<R> rates)
        {
            return AddOrSetRates(rates, false);
        }

        public bool DeleteRatesWhere(Func<_IRate, bool> predicate)
        {
            return marketData.Transact(this, () =>
            {
                var rateCurve = TrinityRateCurve;

                if (rateCurve != null)
                {
                    foreach (var rate in rateCurve.Rates().Where(predicate))
                    {
                        rateCurve.RemoveRate(rate);
                    }

                    rateCurve.Save();
                }
            });
        }

        private bool AddOrSetRates(IEnumerable<R> rates, bool remoteNonMatchingExistingRates = true)
        {
            return marketData.Transact(this, () =>
            {
                var ratesByPeriod = rates.ToDictionary(rate => rate.Period);

                var rateCurve = GetOrCreateTrinityRateCurve();

                foreach (var existingRate in rateCurve.Rates())
                {
                    if (ratesByPeriod.ContainsKey(existingRate.Period))
                    {
                        var newRate = ratesByPeriod[existingRate.Period];

                        newRate.CopyTo(existingRate);

                        ratesByPeriod.Remove(newRate.Period);
                    }
                    else if (remoteNonMatchingExistingRates)
                    {
                        rateCurve.RemoveRate(existingRate);
                    }
                }

                foreach (var missingRate in ratesByPeriod.Values)
                {
                    missingRate.AddTo(rateCurve);
                }

                logger.Info("Saving", rateCurve.Save);
            });
        }
    }
}