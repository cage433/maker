using System;
using MarketData2;

namespace com.trafigura.services.trinity
{
    public class DepoRate : Rate
    {
        public string Period { get; set; }
        public bool PeriodFromToday;
        public double Bid;
        public double Offer;
        public string Date;

        public void AddTo(trRateCurve rateCurve)
        {
            var newRate = rateCurve.NewRate();

            ((trDepoRate) newRate).RateType = treDepoRateType.treDepoRateSimple;

            CopyTo(newRate);
        }

        public void CopyTo(_IRate rate)
        {
            var to = (trDepoRate) rate;

            to.Period = Period;
            to.Bid = Bid;
            to.Off = Offer;
            to.TheDate = Date.ParseDate();
            to.PeriodFromToday = PeriodFromToday;
        }
    }
}