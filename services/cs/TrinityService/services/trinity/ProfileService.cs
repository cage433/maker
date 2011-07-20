using System.ComponentModel.Composition;
using System.ServiceModel;
using System.ServiceModel.Activation;
using MarketData2;

namespace com.trafigura.services.trinity
{
    [Export]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class ProfileService : ProfileServiceApi
    {
        private readonly trMarketDataServer marketData;

        public ProfileService(trMarketDataServer marketData)
        {
            this.marketData = marketData;
        }

        public Profile Get(string name, string visibility)
        {
            return new Profile
            {
                Id = marketData.ProfileByName[visibility, name],
                Name = name,
                Visibility = visibility
            };
        }
    }
}