using System.Windows;

namespace com.trafigura.ui.pivot
{
    /// <summary>
    /// Interaction logic for PivotWindow.xaml
    /// </summary>
    public partial class PivotWindow : Window
    {
        public PivotWindow()
        {
            InitializeComponent();
            cannedDataPivotTable.PTDataSource = new CannedPivotTableDataSource();
        }
    }
}
