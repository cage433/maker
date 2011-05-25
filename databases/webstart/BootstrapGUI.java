package bootstrap;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.Permissions;
import java.security.Policy;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

/**
 * Creates a url classloader using the first argument and then runs Launcher passing in the remaining arguments
 */
public class BootstrapGUI {
	
	public static void main(final String[] args) throws Exception {
        if (args.length < 2) {
            System.err.println("Expected BoostrapGUI <classloaderurl> <mainclass> (arguments,arguments)");
            System.exit(1);
        }
		String url = args[0];
        String mainClass = args[1];
        String[] applicationArguments = Arrays.copyOfRange(args, 2, args.length);
        System.out.println("ClassloaderURL: " + url);
        System.out.println("Main class    : " + mainClass);
        System.out.println("Args          : " + Arrays.asList(applicationArguments));
		try {
            String classesUrl = url + "/classes/";
            final URLClassLoader classLoader = new URLClassLoader(new URL[] { new URL(classesUrl) });

            //Without this line the swing look and feel classes use the webstart classloader
            UIManager.put("ClassLoader", classLoader);

            //trident uses the context classloader on the swing thread so we set it here
            SwingUtilities.invokeLater(new Runnable() { public void run() {
                Thread.currentThread().setContextClassLoader(classLoader);    
            } });

            //all-permissions in the webstart jnlp file only applies to this class loader
            //the following code set the 'all' permission on the urlclassloader
            Policy.setPolicy( new Policy() {
                public PermissionCollection getPermissions(CodeSource codesource) {
                    Permissions perms = new Permissions();
                    perms.add(new AllPermission());
                    return(perms);
                }
                public void refresh(){
                }
            });
            String resourceForMainClass = mainClass.replaceAll("\\.", "/") + ".class";
            if (classLoader.getResource(resourceForMainClass) == null) {
                //loadClass just throws a ClassNotFoundException
                //so we use getResource as an explicit check to see if the server is running
                System.err.println("Can't load resource " + resourceForMainClass + " from " + url + " Is the server running?");
            } else {
                Class launcher = classLoader.loadClass(mainClass);
                Method main = launcher.getMethod("main", new Class[] { String[].class });
                main.invoke(null, new Object[] { applicationArguments});
            }
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}
}
