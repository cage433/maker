using System;
using System.Collections.Generic;
using System.Linq;
using MarketData2;

namespace com.trafigura.services.trinity
{
    public class CommodityCurve : Curve<CommodityRate>
    {
        private readonly string exchange;
        private readonly string currency;

        public CommodityCurve(trMarketDataServer marketData, Profile profile, string exchange, string commodity, string currency)
            : base(marketData, profile, commodity)
        {
            this.exchange = exchange;
            this.currency = currency;
        }

        protected override trRateCurve TrinityRateCurve
        {
            get { return marketData.RateCurve[Profile.Id, Commodity, exchange, trRateCurveTypeEnum.trRateCurveCommodity, currency]; }
        }

        protected override trRateCurve GetOrCreateTrinityRateCurve()
        {
            return TrinityRateCurve ?? marketData.NewRateCurve[Profile.Id, Commodity, exchange, trRateCurveTypeEnum.trRateCurveCommodity, currency];
        }

        public override List<CommodityRate> Rates
        {
            get { return TrinityRateCurve.Rates().Select(Create).OrderBy(rate => rate.OADate()).ToList(); }
        }

        public CommodityRate Create(_IRate rate)
        {
            var commodityRate = (trCommodityRate) rate;
            var spotDate = marketData.ExchangeSpotDate[Profile.Id, Commodity, exchange];

            return new CommodityRate()
            {
                Period = commodityRate.Period,
                Bid = commodityRate.Bid,
                Offer = commodityRate.Off,
                UnitsTop = commodityRate.UnitsTop,
                UnitsBottom = commodityRate.UnitsBot,
                Exchange = commodityRate.Exchange,
                Contract = commodityRate.Contract,
                Date = DateTime.FromOADate(TheDateOrSpot(commodityRate, spotDate)).ToShortDateString()
            };
        }

        public static int TheDateOrSpot(trCommodityRate rate, int spotDate)
        {
            return rate.Period == "CASH" ? spotDate : rate.TheDate;
        }
    }
}