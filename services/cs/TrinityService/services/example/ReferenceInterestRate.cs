namespace com.trafigura.services
{
    public class TitanSerializableDate
    {
        public string Value;
    }

    public class TitanSerializableCurrency
    {
        public string Name;
    }

    public class TitanSerializablePercentage
    {
        public double Value;
    }

    namespace marketdata
    {
        public class ReferenceInterestRate
        {
            public TitanSerializableDate ObservationDate;
            public ReferenceRateSource Source;
            public Maturity Maturity;
            public TitanSerializableCurrency Currency;
            public TitanSerializablePercentage Rate;
        }

        public class ReferenceRateSource
        {
            public string Name;
        }

        public abstract class Maturity { }

        public class NamedMaturity : Maturity
        {
            public NamedMaturityType Name;
        }

        public enum NamedMaturityType
        {
            ON,
            SN
        }

        public class RelativeMaturity : Maturity
        {
            public int Value;
            public RelativeMaturityType MaturityType;
        }

        public class RelativeMaturityType
        {
            public static RelativeMaturityType Day = new RelativeMaturityType { Name = MaturityTypeNames.Day, ShortName = ShortMaturityTypeNames.D };

            public MaturityTypeNames Name;
            public ShortMaturityTypeNames ShortName;
        }

        public enum MaturityTypeNames
        {
            Day,
            Week,
            Month,
            Year
        }

        public enum ShortMaturityTypeNames
        {
            D, W, M, Y
        }
    }
}