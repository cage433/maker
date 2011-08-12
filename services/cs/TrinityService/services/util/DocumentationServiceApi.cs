using System.Collections.Generic;
using System.Net.Http;
using System.ServiceModel;
using System.ServiceModel.Web;
using com.trafigura.services.meta;

namespace com.trafigura.services.util
{
    [ServiceContract]
    public interface DocumentationServiceApi
    {
        [OperationContract]
        [WebGet(UriTemplate = "Files/{filename}")]
        HttpResponseMessage File(string filename);

        [OperationContract]
        [WebGet(UriTemplate = "Meta/{path}")]
        WebService MetaData(string path);

        [OperationContract]
        [WebGet(UriTemplate = "Services")]
        List<WebService> AllMetaData();
    }
}