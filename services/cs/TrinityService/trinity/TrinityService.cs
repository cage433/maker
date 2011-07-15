using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.ServiceModel.Web;
using log4net;
using MarketData2;
using trRoot;

namespace com.trafigura.services.trinity
{
    [Export]
    [ServiceContract]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class TrinityService
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

        [OperationContract]
        [WebGet(UriTemplate = "Profile/{name}/{visibility}")]
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

        [OperationContract]
        [WebGet(UriTemplate = "DepoRateCurve/{profileId}/{commodity}")]
        public List<DepoRate> GetDepoRateCurve(int profileId, string commodity)
        {
            logger.Info(string.Format("GET DepoRateCurve/{0}/{1}", profileId, commodity));

            var rates = new List<DepoRate>();

            var rateCurve = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];

            if (rateCurve != null)
            {
                var rate = rateCurve.FirstRate as trDepoRate;

                while (rate != null)
                {
                    rates.Add(new DepoRate
                    {
                        Period = rate.Period,
                        PeriodFromToday = rate.PeriodFromToday,
                        Bid = rate.Bid,
                        Offer = rate.Off,
                        Date = rate.TheDate
                    });

                    rate = rate.NextRate;
                }
            }

            return rates;
        }

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}", Method = "DELETE")]
        public bool DeleteDepoRateCurve(int profileId, string commodity)
        {
            marketData.Lock(profileId, commodity, () =>
            {
                var rateCurve = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];

                if (rateCurve != null)
                {
                    var rate = rateCurve.FirstRate;

                    while (rate != null)
                    {
                        rateCurve.RemoveRate(rate);
                        rate = rate.NextRate;
                    }

                    rateCurve.Save();

                }
            });

            var ratesLeft = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];

            return ratesLeft == null || ratesLeft.RateCount == 0;
        }

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}", Method = "POST")]
        public List<DepoRate> SetDepoRateCurve(int profileId, string commodity, List<DepoRate> rates)
        {
            marketData.Lock(profileId, commodity, () =>
            {
                trRateCurve rateCurve = marketData.RateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];
                if (rateCurve == null)
                {
                    rateCurve = marketData.NewRateCurve[profileId, commodity, null, trRateCurveTypeEnum.trRateCurveDepo, null];
                }

                foreach (var newRate in rates)
                {
                    var newDepoRate = (trDepoRate) rateCurve.NewRate();

                    newDepoRate.RateType = treDepoRateType.treDepoRateSimple;
                    newDepoRate.Period = newRate.Period;
                    newDepoRate.Bid = newRate.Bid;
                    newDepoRate.Off = newRate.Offer;
                }

                logger.Info("Saving", () => rateCurve.Save());
            });

            return GetDepoRateCurve(profileId, commodity);
        }
    }
}