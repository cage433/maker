using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.ServiceModel.Web;
using log4net;
using MarketData2;
using trRoot;

namespace com.trafigura.services.trinity
{
    [Export]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class TrinityService : TrinityServiceApi
    {
        private readonly ILog logger = LogManager.GetLogger(typeof (TrinityService));

        private readonly trMarketDataServer marketData;

        public TrinityService(TrinityCredentials credentials)
        {
            try
            {
                logger.Info("Logging in", () =>
                {
                    var trcRoot = new trcRoot();
                    trcRoot.Login(credentials.Database, credentials.Username, credentials.Password);
                });
            }
            catch (Exception e)
            {
                throw new Exception(string.Format("Could not log into {0} {1} {2}, {3}",
                    credentials.Database, credentials.Username, credentials.Password, e.Message));
            }
            
            marketData = new trMarketDataServer();
        }

        public Profile GetProfile(string name, string visibility)
        {
            int profileId = marketData.ProfileByName[visibility, name];

            return new Profile
            {
                Id = profileId,
                Name = name,
                Visibility = visibility
            };
        }

        public List<DepoRate> GetDepoRateCurve(int profileId, string commodity)
        {
            var rateCurve = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];

            return rateCurve.DepoRates().Select(rate => new DepoRate
            {
                Period = rate.Period,
                PeriodFromToday = rate.PeriodFromToday,
                Bid = rate.Bid,
                Offer = rate.Off,
                Date = rate.TheDate
            }).ToList();
        }

        public bool DeleteDepoRateCurve(int profileId, string commodity)
        {
            return DeleteDepoRateWhere(profileId, commodity, rate => true);
        }

        public bool DeleteDepoRate(int profileId, string commodity, string period)
        {
            return DeleteDepoRateWhere(profileId, commodity, rate => rate.Period == period);
        }

        public bool SetDepoRateCurve(int profileId, string commodity, List<DepoRate> rates)
        {
            return marketData.Transact(profileId, commodity, () =>
            {
                var rateCurve = GetOrCreate(profileId, commodity);

                foreach (var newRate in rates)
                {
                    rateCurve.NewRate(newRate);
                }

                logger.Info("Saving", rateCurve.Save);
            });
        }

        public bool AddDepoRates(int profileId, string commodity, List<DepoRate> rates)
        {
            return marketData.Transact(profileId, commodity, () =>
            {
                var ratesByPeriod = rates.ToDictionary(rate => rate.Period);

                var rateCurve = GetOrCreate(profileId, commodity);

                foreach (var existingRate in rateCurve.DepoRates().Where(depoRate => ratesByPeriod.ContainsKey(depoRate.Period)))
                {
                    var newRate = ratesByPeriod[existingRate.Period];

                    existingRate.Copy(newRate);

                    ratesByPeriod.Remove(newRate.Period);
                }

                foreach (var missingRate in ratesByPeriod.Values)
                {
                    rateCurve.NewRate(missingRate);
                }

                logger.Info("Saving", rateCurve.Save);
            });
        }

        private bool DeleteDepoRateWhere(int profileId, string commodity, Func<_IRate, bool> predicate)
        {
            return marketData.Transact(profileId, commodity, () =>
            {
                var rateCurve = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];

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

        private trRateCurve GetOrCreate(int profileId, string commodity)
        {
            return marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null] 
             ?? marketData.NewRateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];
        }
    }
}