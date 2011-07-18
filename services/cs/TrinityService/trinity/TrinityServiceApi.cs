using System.Collections.Generic;
using System.ServiceModel;
using System.ServiceModel.Web;

namespace com.trafigura.services.trinity
{
    [ServiceContract]
    public interface TrinityServiceApi
    {
        [OperationContract]
        [WebGet(UriTemplate = "Profile/{name}/{visibility}")]
        Profile GetProfile(string name, string visibility);

        [OperationContract]
        [WebGet(UriTemplate = "DepoRateCurve/{profileId}/{commodity}")]
        List<DepoRate> GetDepoRateCurve(int profileId, string commodity);

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}", Method = "DELETE")]
        bool DeleteDepoRateCurve(int profileId, string commodity);

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}/{period}", Method = "DELETE")]
        bool DeleteDepoRate(int profileId, string commodity, string period);

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}", Method = "POST")]
        bool SetDepoRateCurve(int profileId, string commodity, List<DepoRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "DepoRateCurve/{profileId}/{commodity}", Method = "PUT")]
        bool AddDepoRates(int profileId, string commodity, List<DepoRate> rates);
    }
}