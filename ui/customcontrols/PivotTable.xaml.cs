using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using PivotGridLibrary;

namespace com.trafigura.ui.customcontrols
{
    /// <summary>
    /// Interaction logic for PivotTable.xaml
    /// This is a WPF pivot table control that wraps the SyncFusion WinForms pivot table.
    /// If anything it aims to reduce the number of configurable properties in the SyncFusion pivot 
    /// to only those we really need to change.
    /// </summary>
    public partial class PivotTable : UserControl
    {
        public static readonly DependencyProperty HeadingsBackColorProperty 
            = DependencyProperty.Register("HeadingsBackColor",
                typeof(Color), typeof(PivotTable), 
                new FrameworkPropertyMetadata(new PropertyChangedCallback(HeadingsBackColorChanged)));

        public static readonly DependencyProperty TotalsBackColorProperty
            = DependencyProperty.Register("TotalsBackColor",
                typeof(Color), typeof(PivotTable),
                new FrameworkPropertyMetadata(new PropertyChangedCallback(TotalsBackColorChanged)));

        public static readonly DependencyProperty ExpandCellsBackColorProperty
            = DependencyProperty.Register("ExpandCellsBackColor",
                typeof(Color), typeof(PivotTable),
                new FrameworkPropertyMetadata(new PropertyChangedCallback(ExpandCellsBackColorChanged)));

        public static readonly DependencyProperty TotalsTextColorProperty
            = DependencyProperty.Register("TotalsTextColor",
                typeof(Color), typeof(PivotTable),
                new FrameworkPropertyMetadata(new PropertyChangedCallback(TotalsTextColorChanged)));

        public static readonly DependencyProperty CalculationCellBackColorProperty
            = DependencyProperty.Register("CalculationCellBackColor",
                typeof(Color), typeof(PivotTable),
                new FrameworkPropertyMetadata(new PropertyChangedCallback(CalculationCellBackColorChanged)));

        public static readonly DependencyProperty HeadingsBoldProperty
                 = DependencyProperty.Register("HeadingsBold",
        typeof(bool), typeof(PivotTable),
               new FrameworkPropertyMetadata(new PropertyChangedCallback(HeadingsBoldChanged)));

        public PivotTable()
        {
            InitializeComponent();
        }

        public PivotTableDataSource PTDataSource
        {
           set { pivotTableControl.DataSource = (object)value.GetData(); }  
        }

        public static void HeadingsBackColorChanged(DependencyObject source,DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            System.Drawing.Color color = EventToNewDrawingColor(e);
            appearance.RowCell.BackColor = color;
            appearance.ColumnCell.BackColor = color;
        }

        public static void TotalsBackColorChanged(DependencyObject source, DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            System.Drawing.Color color = EventToNewDrawingColor(e);
            appearance.ColumnTotalCell.BackColor = color;
            appearance.RowTotalCell.BackColor = color;
        }

        public static void ExpandCellsBackColorChanged(DependencyObject source, DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            System.Drawing.Color color = EventToNewDrawingColor(e);
            appearance.RowExpandCell.BackColor = color;
            appearance.ColumnExpandCell.BackColor = color;
        }

        public static void TotalsTextColorChanged(DependencyObject source, DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            System.Drawing.Color color = EventToNewDrawingColor(e);
            appearance.ColumnTotalCell.TextColor = color;
            appearance.RowTotalCell.TextColor = color;
        }

        public static void CalculationCellBackColorChanged(DependencyObject source, DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            appearance.CalculationNameCell.BackColor = EventToNewDrawingColor(e);
        }

        public static void HeadingsBoldChanged(DependencyObject source, DependencyPropertyChangedEventArgs e)
        {
            PivotAppearance appearance = (source as PivotTable).pivotTableControl.GetAppearance();
            bool bold = (bool)e.NewValue;
            appearance.RowCell.Font.Bold = bold;
            appearance.ColumnCell.Font.Bold = bold;
            appearance.ColumnTotalCell.Font.Bold = bold;
            appearance.RowTotalCell.Font.Bold = bold;
            appearance.RowExpandCell.Font.Bold = bold;
            appearance.ColumnExpandCell.Font.Bold = bold;
        } 

        public Color HeadingsBackColor
        {
            get  { return (Color)GetValue(HeadingsBackColorProperty); }
            set  { SetValue(HeadingsBackColorProperty, value); }
        }

        public Color TotalsBackColor
        {
            get { return (Color)GetValue(TotalsBackColorProperty); }
            set { SetValue(TotalsBackColorProperty, value); }
        }

        public Color ExpandCellsBackColor
        {
            get { return (Color)GetValue(ExpandCellsBackColorProperty); }
            set { SetValue(ExpandCellsBackColorProperty, value); }
        }

        public Color TotalsTextColor
        {
            get { return (Color)GetValue(TotalsTextColorProperty); }
            set { SetValue(TotalsTextColorProperty, value); }
        }

        public Color CalculationCellBackColor
        {
            get { return (Color)GetValue(CalculationCellBackColorProperty); }
            set { SetValue(CalculationCellBackColorProperty, value); }
        }

        public bool HeadingsBold
        {
            get { return (bool)GetValue(HeadingsBoldProperty); }
            set { SetValue(HeadingsBoldProperty, value); }
        }

        private static System.Drawing.Color EventToNewDrawingColor(DependencyPropertyChangedEventArgs e)
        {
            Color newColor = (Color)e.NewValue;
            return ToDrawingColor(newColor);
        }

        // Converts from the default color type used by WPF to that used
        // by the old WinForms pivot table
        private static System.Drawing.Color ToDrawingColor(Color color)
        {
            return System.Drawing.Color.FromArgb(color.A, color.R, color.G, color.B);
        }
    }
}
