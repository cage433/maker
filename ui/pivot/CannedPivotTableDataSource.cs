using System;
using System.Data;
using com.trafigura.ui.customcontrols;

namespace com.trafigura.ui.pivot
{
    class CannedPivotTableDataSource : PivotTableDataSource
    {
        private readonly static string[] TRADERS 
            = new string[] { "Corin Adlard", "Brian Agnew", "Keith Davies", "Alex McGuire" };
        private static string[] LOCATIONS = new string[] { "London", "Ulan Baatar", "Burnley"};
        private static string[] TRADE_TYPES = new string[] { "Future", "Swap", "Option" };
        private static string[] EXCHANGES = new string[] { "CME", "CBOT", "LME" };

        private DataTable _data;

        public CannedPivotTableDataSource()
        {
            _data = new DataTable();
            Random r = new Random(123345345);
            _data.Columns.Add(new DataColumn("Trader", typeof(string)));
            _data.Columns.Add(new DataColumn("Location", typeof(string)));
            _data.Columns.Add(new DataColumn("TradeType", typeof(string)));
            _data.Columns.Add(new DataColumn("Exchange", typeof(string)));
            _data.Columns.Add(new DataColumn("Position", typeof(int)));
            _data.Columns.Add(new DataColumn("PV", typeof(double)));
            for (int i = 0; i < 2000; ++i)
            {
                DataRow row = _data.NewRow();
                row["Trader"] = getRandomFromList(r, TRADERS);
                row["Location"] = getRandomFromList(r, LOCATIONS);
                row["TradeType"] = getRandomFromList(r, TRADE_TYPES);
                row["Exchange"] = getRandomFromList(r, EXCHANGES);
                row["Position"] = r.Next(1000000)-1000000/2;
                row["PV"] = r.Next(1000000);
                _data.Rows.Add(row);
            }
        }

        private string getRandomFromList(Random random, string[] selection)
        {
            int num = selection.GetLength(0);
            return selection[random.Next(num)];
        }

        public DataTable GetData()
        {
            return _data;
        }
    }
}
