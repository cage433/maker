using System;
using com.trafigura.services.util;
using log4net;
using MarketData2;
using trRoot;

namespace com.trafigura.services.trinity
{
    public class TrinityServices
    {
        private readonly ILog logger = LogManager.GetLogger(typeof (TrinityServices));

        public TrinityServices(TrinityCredentials credentials)
        {
            try
            {
                logger.Info(string.Format("Logging in to: {0} as: {1} ", credentials.Database, credentials.Username), () =>
                {
                    var trcRoot = logger.Info("Instantiating trcRoot", () => new trcRoot());

                    trcRoot.Login(credentials.Database, credentials.Username, credentials.Password);
                });
            }
            catch (Exception e)
            {
                throw new Exception(string.Format("Could not log into {0} {1} {2}, {3}",
                    credentials.Database, credentials.Username, credentials.Password, e.Message));
            }
        }

        public void MapServiceRoues(RouteRegistry routeRegistry)
        {
            var marketData = new trMarketDataServer();

            routeRegistry.MapServiceRoute("Profile", new ProfileService(marketData))
                         .MapServiceRoute("DepoRate", new DepoRatesService(marketData))
                         .MapServiceRoute("CommodityRate", new CommodityRatesService(marketData));
        }
    }
}