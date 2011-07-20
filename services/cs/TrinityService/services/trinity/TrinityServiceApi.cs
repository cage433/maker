using System.Collections.Generic;
using System.ServiceModel;
using System.ServiceModel.Web;
using com.trafigura.services.util;

namespace com.trafigura.services.trinity
{
    [ServiceContract]
    public interface ProfileServiceApi
    {
        [OperationContract]
        [WebGet(UriTemplate = "/{name}/{visibility}")]
        [Example("Full Curve/public")]
        Profile Get(string name, string visibility);
    }

    [ServiceContract]
    public interface DepoRatesServiceApi
    {
        [OperationContract]
        [Example("11/USD")]
        [WebGet(UriTemplate = "/{profileId}/{commodity}")]
        List<DepoRate> GetRates(int profileId, string commodity);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{commodity}", Method = "DELETE")]
        bool DeleteRates(int profileId, string commodity);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{commodity}/{period}", Method = "DELETE")]
        bool DeleteRate(int profileId, string commodity, string period);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{commodity}", Method = "POST")]
        bool SetRates(int profileId, string commodity, List<DepoRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{commodity}", Method = "PUT")]
        bool AddRates(int profileId, string commodity, List<DepoRate> rates);
    }

    [ServiceContract]
    public interface CommodityRatesServicesApi
    {
        [OperationContract]
        [Example("11/SFE/XPS/USD")]
        [WebGet(UriTemplate = "/{profileId}/{exchange}/{commodity}/{currency}")]
        List<CommodityRate> GetRates(int profileId, string exchange, string commodity, string currency);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{exchange}/{commodity}/{currency}", Method = "DELETE")]
        bool DeleteRates(int profileId, string exchange, string commodity, string currency);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{exchange}/{commodity}/{currency}/{period}", Method = "DELETE")]
        bool DeleteRate(int profileId, string exchange, string commodity, string currency, string period);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{exchange}/{commodity}/{currency}", Method = "POST")]
        bool SetRates(int profileId, string exchange, string commodity, string currency, List<CommodityRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "/{profileId}/{exchange}/{commodity}/{currency}", Method = "PUT")]
        bool AddRates(int profileId, string exchange, string commodity, string currency, List<CommodityRate> rates);
    }
}