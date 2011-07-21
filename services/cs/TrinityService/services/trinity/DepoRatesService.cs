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
    public class DepoRatesService : DepoRatesServiceApi
    {
        private readonly trMarketDataServer marketData;

        public DepoRatesService(trMarketDataServer marketData)
        {
            this.marketData = marketData;
        }

        public List<DepoRate> GetRates(int profileId, string commodity)
        {
            return new DepoCurve(marketData, profileId, commodity).Rates;
        }

        public bool DeleteRates(int profileId, string commodity)
        {
            return new DepoCurve(marketData, profileId, commodity).DeleteRatesWhere(rate => true);
        }

        public bool DeleteRate(int profileId, string commodity, string period)
        {
            return new DepoCurve(marketData, profileId, commodity).DeleteRatesWhere(rate => rate.Period == period);
        }

        public bool SetRates(int profileId, string commodity, List<DepoRate> rates)
        {
            return new DepoCurve(marketData, profileId, commodity).SetRates(rates);
        }

        public bool AddRates(int profileId, string commodity, List<DepoRate> rates)
        {
            return new DepoCurve(marketData, profileId, commodity).AddRates(rates);
        }
    }
}