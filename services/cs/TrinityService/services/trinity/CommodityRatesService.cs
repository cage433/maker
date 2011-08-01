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
        private readonly ProfileService profileService;

        public CommodityRatesService(trMarketDataServer marketData)
        {
            this.marketData = marketData;
            profileService = new ProfileService(marketData);
        }

        public List<CommodityRate> GetRates(string exchange, string commodity, string currency, string profileName)
        {
            return CommodityCurve(exchange, commodity, currency, profileName).Rates;
        }

        public bool DeleteRates(string exchange, string commodity, string currency, string profileName)
        {
            return CommodityCurve(exchange, commodity, currency, profileName).DeleteRatesWhere(rate => true);
        }

        public bool DeleteRate(string exchange, string commodity, string currency, string period, string profileName)
        {
            return CommodityCurve(exchange, commodity, currency, profileName).DeleteRatesWhere(rate => rate.Period == period);
        }

        public bool SetRates(string exchange, string commodity, string currency, string profileName, List<CommodityRate> rates)
        {
            return CommodityCurve(exchange, commodity, currency, profileName).SetRates(rates);
        }

        public bool AddRates(string exchange, string commodity, string currency, string profileName, List<CommodityRate> rates)
        {
            return CommodityCurve(exchange, commodity, currency, profileName).AddRates(rates);
        }

        private CommodityCurve CommodityCurve(string exchange, string commodity, string currency, string profileName)
        {
            return new CommodityCurve(marketData, profileService.Get(profileName, "public"), exchange, commodity, currency);
        }
    }
}