using System.ComponentModel.Composition;
using System.Linq;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.Text.RegularExpressions;
using MarketData2;
using System;

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
            Match match = Regex.Match(name, @"(.*) \((.*)\)");

            if (match.Success)
            {
                return GetByDate(match.Groups[1].Value, visibility, match.Groups[2].Value);
            }

            return GetByName(name, visibility);
        }

        public Profile GetByDate(string name, string visibility, string date)
        {
            return new Profile
            {
                Id = marketData.ProfileInstanceByEffectiveDate[GetByName(name, visibility).Id, date.ParseDate()],
                Name = name,
                Visibility = visibility
            };
        }

        private Profile GetByName(string name, string visibility)
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