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

        public int ProfileId { get; private set; }
        public string Commodity { get; private set; }

        protected Curve(trMarketDataServer marketData, int profileId, string commodity)
        {
            this.marketData = marketData;
            ProfileId = profileId;
            Commodity = commodity;
        }

        protected abstract trRateCurve TrinityRateCurve { get; }
        protected abstract trRateCurve GetOrCreateTrinityRateCurve();

        public abstract List<R> Rates { get; }

        public bool SetRates(List<R> rates)
        {
            return DeleteRatesWhere(rate => true) && marketData.Transact(this, () =>
            {
                var rateCurve = GetOrCreateTrinityRateCurve();

                foreach (var newRate in rates)
                {
                    newRate.AddTo(rateCurve);
                }

                logger.Info("Saving", rateCurve.Save);
            });
        }

        public bool AddRates(List<R> rates)
        {
            return marketData.Transact(this, () =>
            {
                var ratesByPeriod = rates.ToDictionary(rate => rate.Period);

                var rateCurve = GetOrCreateTrinityRateCurve();

                foreach (var existingRate in rateCurve.Rates().Where(rate => ratesByPeriod.ContainsKey(rate.Period)))
                {
                    var newRate = ratesByPeriod[existingRate.Period];

                    newRate.CopyTo(existingRate);

                    ratesByPeriod.Remove(newRate.Period);
                }

                foreach (var missingRate in ratesByPeriod.Values)
                {
                    missingRate.AddTo(rateCurve);
                }

                logger.Info("Saving", rateCurve.Save);
            });
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
    }
}