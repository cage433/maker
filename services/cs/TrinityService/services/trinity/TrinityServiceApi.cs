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
        [WebGet(UriTemplate = "{name}/{visibility}")]
        [Example("Full Curve/public")]
        Profile Get(string name, string visibility);

        [OperationContract]
        [WebGet(UriTemplate = "{name}/{visibility}/{date}")]
        [Example("Full Curve/public/26-07-2011")]
        Profile GetByDate(string name, string visibility, string date);
    }

    [ServiceContract]
    public interface DepoRatesServiceApi
    {
        [OperationContract]
        [Example("USD/Full Curve")]
        [WebGet(UriTemplate = "{commodity}/{profileName}")]
        List<DepoRate> GetRates(string commodity, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{commodity}/{profileName}", Method = "DELETE")]
        bool DeleteRates(string commodity, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{commodity}/{period}/{profileName}", Method = "DELETE")]
        bool DeleteRate(string commodity, string period, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{commodity}/{profileName}", Method = "POST")]
        bool SetRates(string commodity, string profileName, List<DepoRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "{commodity}/{profileName}", Method = "PUT")]
        bool AddRates(string commodity, string profileName, List<DepoRate> rates);
    }

    [ServiceContract]
    public interface CommodityRatesServicesApi
    {
        [OperationContract]
        [Example("11/SFE/XPS/USD")]
        [WebGet(UriTemplate = "{exchange}/{commodity}/{currency}/{profileName}")]
        List<CommodityRate> GetRates(string exchange, string commodity, string currency, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{exchange}/{commodity}/{currency}/{profileName}", Method = "DELETE")]
        bool DeleteRates(string exchange, string commodity, string currency, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{exchange}/{commodity}/{currency}/{period}/{profileName}", Method = "DELETE")]
        bool DeleteRate(string exchange, string commodity, string currency, string period, string profileName);

        [OperationContract]
        [WebInvoke(UriTemplate = "{exchange}/{commodity}/{currency}/{profileName}", Method = "POST")]
        bool SetRates(string exchange, string commodity, string currency, string profileName, List<CommodityRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "{exchange}/{commodity}/{currency}/{profileName}", Method = "PUT")]
        bool AddRates(string exchange, string commodity, string currency, string profileName, List<CommodityRate> rates);
    }
}