using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace com.trafigura.ui.customcontrols
{
    /// <summary>
    /// Interaction logic for CheckedListBox.xaml
    /// </summary>
    public partial class CheckedListBox : UserControl
    {
        public CheckedListBox()
        {
            InitializeComponent();
        }

        public static readonly DependencyProperty ItemsSourceProperty = DependencyProperty.Register(
            "ItemsSource", typeof(IEnumerable<object>), typeof(CheckedListBox));
        public static readonly DependencyProperty DragEnabledProperty =
            DependencyProperty.RegisterAttached("DragEnabled", typeof(Boolean),
               typeof(CheckedListBox), new FrameworkPropertyMetadata(OnDragEnabledChanged));

        public IEnumerable<object> ItemsSource
        {
            get
            {
                return (IEnumerable<object>)GetValue(ItemsSourceProperty);
            }
            set
            {
                SetValue(ItemsSourceProperty, value);
            }
        }
 
        public static void SetDragEnabled(DependencyObject element, Boolean value)
        {
            element.SetValue(DragEnabledProperty, value);
        }

        public static Boolean GetDragEnabled(DependencyObject element)
        {
            return (Boolean)element.GetValue(DragEnabledProperty);
        }

        public static void OnDragEnabledChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            if ((bool)args.NewValue)
            {
                CheckedListBox listBox = (CheckedListBox)obj;
                ListBox innerListBox = listBox.innerListBox;
                listBox.PreviewMouseLeftButtonDown +=
                    new MouseButtonEventHandler(_PreviewMouseLeftButtonDown);
            }
        }

        private static void _PreviewMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            CheckedListBox listBox = (CheckedListBox)sender;
            ListBox innerListBox = listBox.innerListBox;
            object data = (object)GetObjectDataFromPoint(innerListBox, e.GetPosition(innerListBox));
            if (data != null)
            {
                string data2 = "hi";
                DragDrop.DoDragDrop(innerListBox, data2, DragDropEffects.Copy);
            }
        }

        private static object GetObjectDataFromPoint(ListBox source, Point point)
        {
            UIElement element = source.InputHitTest(point) as UIElement;
            if (element != null)
            {
                object data = DependencyProperty.UnsetValue;
                while (data == DependencyProperty.UnsetValue)
                {
                    data = source.ItemContainerGenerator.ItemFromContainer(element);
                    if (data == DependencyProperty.UnsetValue)
                        element = VisualTreeHelper.GetParent(element) as UIElement;
                    if (element == source)
                        return null;
                }
                if (data != DependencyProperty.UnsetValue)
                    return data;
            }
            return null;
        }

/*        private void ListboxPreviewMouseMove(object sender, MouseEventArgs e)
          {
            ListBox listbox = (ListBox)sender;
            bool isDown = (bool)listbox.GetValue(IsDownProperty);
            if (!isDown)
            {
                return;
            }
            Point startPoint = (Point)listbox.GetValue(StartPointProperty);
            if (Math.Abs(e.GetPosition(listbox).X - startPoint.X) > SystemParameters.MinimumHorizontalDragDistance ||
                Math.Abs(e.GetPosition(listbox).Y - startPoint.Y) > SystemParameters.MinimumVerticalDragDistance)
            {
                DragStarted(listbox);
            }
        }*/
    }
}
