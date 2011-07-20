using MarketData2;

namespace com.trafigura.services.trinity
{
    public interface Rate
    {
        void AddTo(trRateCurve rateCurve);
        void CopyTo(_IRate rate);
        string Period { get; }
    }
}