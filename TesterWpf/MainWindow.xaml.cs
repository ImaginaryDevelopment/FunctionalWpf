using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using WpfTypes;

namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            this.DataContext = new WpfComponents.LoginComponent.LoginCredential(); //.LoginCredential();
            InitializeComponent();
            //this.DataContext = new LoginCredential();
        }

        void Button_Click(object sender, RoutedEventArgs e)
        {
            var blob = WpfComponents.LoginComponent.makeLoginWindow();
            if (blob.IsChoice1Of3 || blob.IsChoice2Of3)
            {
                var isCompleteSuccess = blob.IsChoice1Of3;
                var x = (blob.getMaybe2Of3() ?? blob.getMaybe1Of3())?.Value;
                var w = x.Item1;
                var xaml = x.Item2;
                var getter = x.Item3;

                if (w.ShowDialog().GetValueOrDefault())
                {
                    if (x.Item3 != null)
                    {
                        var cred = x.Item3.Value;
                        Console.WriteLine(cred.ToString());
                    } else
                    {
                        Console.WriteLine("Getter was null");
                    }
                }
                else
                {
                    Console.WriteLine("ShowDialog returned something else");
                }
            }
            else
            {
                Console.WriteLine("blob is Choice2Of2");
            }
        }
    }
}
