using System.Collections.Generic;
using System.Linq;
using com.trafigura.services.trinity;

namespace MarketData2
{
    public static class trRateCurveExtensions
    {
        public static IEnumerable<_IRate> Rates(this trRateCurve rateCurve)
        {
            if (rateCurve != null)
            {
                var rate = rateCurve.FirstRate;

                while (rate != null)
                {
                    yield return rate;

                    rate = rate.NextRate;
                }
            }
        }

        public static IEnumerable<trDepoRate> DepoRates(this trRateCurve rateCurve)
        {
            return Rates(rateCurve).Select(rate => rate as trDepoRate).Where(depoRate => depoRate != null);
        }

        public static trDepoRate NewRate(this trRateCurve rateCurve, DepoRate from)
        {
            var to = (trDepoRate) rateCurve.NewRate();

            to.RateType = treDepoRateType.treDepoRateSimple;

            return to.Copy(from);
        }
    }

    public static class trDepoRateExtensions
    {
        public static trDepoRate Copy(this trDepoRate to, DepoRate from)
        {
            to.Period = from.Period;
            to.Bid = from.Bid;
            to.Off = from.Offer;
            to.PeriodFromToday = from.PeriodFromToday;

            return to;
        }
    }
}