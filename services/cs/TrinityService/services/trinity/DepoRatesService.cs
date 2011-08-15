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
        private readonly ProfileServiceApi profileService;

        public DepoRatesService(trMarketDataServer marketData)
        {
            this.marketData = marketData;
            this.profileService = new ProfileService(marketData);
        }

        public List<DepoRate> GetRates(string commodity, string profileName)
        {
            return DepoCurve(commodity, profileName).Rates;
        }

        public bool DeleteRates(string commodity, string profileName)
        {
            return DepoCurve(commodity, profileName).DeleteRatesWhere(rate => true);
        }

        public bool DeleteRate(string commodity, string period, string profileName)
        {
            return DepoCurve(commodity, profileName).DeleteRatesWhere(rate => rate.Period == period);
        }

        public bool SetRates(string commodity, string profileName, List<DepoRate> rates)
        {
            return DepoCurve(commodity, profileName).SetRates(rates);
        }

        public bool AddRates(string commodity, string profileName, List<DepoRate> rates)
        {
            return DepoCurve(commodity, profileName).AddRates(rates);
        }

        private DepoCurve DepoCurve(string commodity, string profileName)
        {
            return new DepoCurve(marketData, profileService.Get(profileName, "public"), commodity);
        }
    }
}