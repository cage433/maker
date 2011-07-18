using System.Collections.Generic;
using System.ServiceModel;
using System.ServiceModel.Web;
using com.trafigura.services.marketdata;

namespace com.trafigura.services.example
{
    [ServiceContract]
    public interface ExampleServiceApi
    {
        [OperationContract]
        [WebGet(UriTemplate = "ReferenceInterestRate/{source}")]
        ReferenceInterestRate GetReferenceInterestRate(string source);

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRate/{source}", Method = "POST")]
        ReferenceInterestRate SetReferenceInterestRate(string source, ReferenceInterestRate rate);

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRates", Method = "POST")]
        List<ReferenceInterestRate> SetReferenceInterestRates(List<ReferenceInterestRate> rates);

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRate/{source}", Method = "DELETE")]
        bool DeleteReferenceInterestRate(string source);
    }
}