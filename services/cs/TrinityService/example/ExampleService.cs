using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.ServiceModel.Web;
using com.trafigura.services.marketdata;
using log4net;

namespace com.trafigura.services.example
{
    [Export]
    [ServiceContract]
    [ServiceBehavior(IncludeExceptionDetailInFaults = true)]
    [AspNetCompatibilityRequirements(RequirementsMode = AspNetCompatibilityRequirementsMode.Allowed)]
    public class ExampleService
    {
        private readonly ILog logger = LogManager.GetLogger(typeof(ExampleService));
        private readonly IDictionary<string, ReferenceInterestRate> rates = new Dictionary<string, ReferenceInterestRate>();

        [OperationContract]
        [WebGet(UriTemplate = "ReferenceInterestRate/{source}")]
        public ReferenceInterestRate GetReferenceInterestRate(string source)
        {
            return rates.GetOrInitialise(source, CreateNew);
        }

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRate/{source}", Method = "POST")]
        public ReferenceInterestRate SetReferenceInterestRate(string source, ReferenceInterestRate rate)
        {
            rates[source] = rate;

            return rate;
        }

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRates", Method = "POST")]
        public List<ReferenceInterestRate> SetReferenceInterestRates(List<ReferenceInterestRate> rates)
        {
            logger.Error("POST ReferenceInterestRates called");

            foreach (var rate in rates)
            {
                this.rates[rate.Source.Name] = rate;
            }

            rates.Reverse();

            return rates;
        }

        [OperationContract]
        [WebInvoke(UriTemplate = "ReferenceInterestRate/{source}", Method = "DELETE")]
        public bool DeleteReferenceInterestRate(string source)
        {
            logger.Info(string.Format("DELETE ReferenceInterestRate/{0} called", source));

//            throw new Exception("BOOM"););

            return rates.Remove(source);
        }

        private static ReferenceInterestRate CreateNew(string source)
        {
            return new ReferenceInterestRate
            {
                ObservationDate = new TitanSerializableDate { Value = "01Jul2011" },
                Source = new ReferenceRateSource { Name = source },
                Maturity = new RelativeMaturity { Value = 1, MaturityType = RelativeMaturityType.Day },
                Currency = new TitanSerializableCurrency { Name = "JPY" },
                Rate = new TitanSerializablePercentage { Value = 1.235 }
            };
        }
    }
}