using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ServiceModel;
using System.ServiceModel.Activation;
using MarketData2;

namespace com.trafigura.services.trinity
{
    [Export]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class CommodityRatesService : CommodityRatesServicesApi
    {
        private readonly trMarketDataServer marketData;

        public CommodityRatesService(trMarketDataServer marketData)
        {
            this.marketData = marketData;
        }

        public List<CommodityRate> GetRates(int profileId, string exchange, string commodity, string currency)
        {
            return new CommodityCurve(marketData, profileId, exchange, commodity, currency).Rates;
        }

        public bool DeleteRates(int profileId, string exchange, string commodity, string currency)
        {
            return new CommodityCurve(marketData, profileId, exchange, commodity, currency).DeleteRatesWhere(rate => true);
        }

        public bool DeleteRate(int profileId, string exchange, string commodity, string currency, string period)
        {
            return new CommodityCurve(marketData, profileId, exchange, commodity, currency).DeleteRatesWhere(rate => rate.Period == period);
        }

        public bool SetRates(int profileId, string exchange, string commodity, string currency, List<CommodityRate> rates)
        {
            return new CommodityCurve(marketData, profileId, exchange, commodity, currency).SetRates(rates);
        }

        public bool AddRates(int profileId, string exchange, string commodity, string currency, List<CommodityRate> rates)
        {
            return new CommodityCurve(marketData, profileId, exchange, commodity, currency).AddRates(rates);
        }
    }
}