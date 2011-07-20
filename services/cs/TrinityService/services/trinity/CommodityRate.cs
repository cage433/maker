using System;
using MarketData2;

namespace com.trafigura.services.trinity
{
    public class CommodityRate : Rate
    {
        public string Period { get; set; }
        public double Bid;
        public double Offer;
        public string Date;
        public string Exchange;
        public string UnitsTop;
        public string UnitsBottom;
        public string Contract;

        public void AddTo(trRateCurve rateCurve)
        {
            var newRate = rateCurve.NewRate();

            ((trCommodityRate)newRate).RateType = trCommodityRateTypeEnum.trCommodityRateForward;

            CopyTo(newRate);
        }

        public void CopyTo(_IRate rate)
        {
            var to = (trCommodityRate) rate;
            to.Period = Period;
            to.Bid = Bid;
            to.Off = Offer;
            to.TheDate = OADate();
            to.UnitsTop = UnitsTop;
            to.UnitsBot = UnitsBottom;
            to.Exchange = Exchange;
            to.Contract = Contract;
        }

        public int OADate()
        {
            return (int) DateTime.Parse(Date).ToOADate();
        }
    }
}