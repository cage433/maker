package starling.booter;

import javax.swing.*;
import java.io.*;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.*;
import java.util.*;

/**
 * Downloads
 */
class Booter {
    public static void main(String[] args) throws Exception {
        if (args.length != 2) {
            System.out.println("Usage: java -jar booter.jar <appurl> <appname>");
            Thread.sleep(5000);
            System.exit(1);
        }
        URL appBaseURL = new URL(args[0]);
        String appName = args[1];
        File tmp = new File(System.getProperty("java.io.tmpdir"));
        File parentCacheDir = new File(tmp, "starling-cache");
        if (!parentCacheDir.exists()) parentCacheDir.mkdir();

        String cacheDirName = appName + "-" +
                appBaseURL.getHost() +
                (appBaseURL.getPort() == 80 ? "" : ("-" + appBaseURL.getPort())) +
                (appBaseURL.getPath().equals("") ? "" : "-" + appBaseURL.getPath());

        File cacheDir = new File(parentCacheDir, cacheDirName.replaceAll("/", "-"));
        if (!cacheDir.exists()) cacheDir.mkdir();
        File logFile = new File(cacheDir, "log.txt");
        System.setOut(new java.io.PrintStream(new TeeOutputStream(System.out, new FileOutputStream(logFile))));
        System.out.println("Cachdir: " + cacheDir.getAbsolutePath());
        run(cacheDir, appBaseURL);
    }

    public static void run(File cacheDir, URL baseURL) throws Exception {

        //HACK this should probably be specified in app.txt
        System.setProperty("log4j.configuration", "utils/resources/log4j.properties");

        URL appURL = new URL(baseURL + "/webstart/app.txt");


        BufferedReader reader;
        try {
            reader = new BufferedReader(new InputStreamReader(appURL.openStream()));
        } catch (final Throwable t) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    String errorMessage = "Cannot connect to the Starling server\nPlease ensure it is running and try again";
                    String title = "Cannot connect to the Starling server";
                    JOptionPane.showMessageDialog(null, errorMessage, title, JOptionPane.ERROR_MESSAGE);
                }
            });
            return;
        }
        java.util.List<String> lines = new LinkedList<String>();
        {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        String[] firstLine = lines.get(0).split(" ");
        String mainClass = firstLine[0];
        String[] applicationArguments = Arrays.copyOfRange(firstLine, 1, firstLine.length);
        Map<String, Long> remoteJarsToTimestamps = new HashMap<String, Long>();
        for (String line : lines.subList(1, lines.size())) {
            String jar = line.split(" ")[0];
            Long timestamp = new Long(line.split(" ")[1]);
            remoteJarsToTimestamps.put(jar, timestamp);
        }

        Map<String, Long> localJarsToTimestamps = new HashMap<String, Long>();
        for (File file : cacheDir.listFiles()) {
            localJarsToTimestamps.put(file.getName(), file.lastModified());
        }

        Map<String, Long> missingOrOutOfDateJars = new HashMap<String, Long>();

        for (Map.Entry<String, Long> entry : remoteJarsToTimestamps.entrySet()) {
            String jar = entry.getKey();
            Long timestamp = entry.getValue();
            if (!localJarsToTimestamps.containsKey(jar) || !timestamp.equals(localJarsToTimestamps.get(jar))) {
                missingOrOutOfDateJars.put(jar, timestamp);
            }
        }

        for (Map.Entry<String, Long> entry : missingOrOutOfDateJars.entrySet()) {
            String jar = entry.getKey();
            Long timestamp = entry.getValue();
            File file = new File(cacheDir, jar);
            URL url = new URL(baseURL + "/webstart/" + jar);
            System.out.println("downloading " + url);
            InputStream inputStream = url.openStream();
            OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(file));
            copy(inputStream, outputStream);
            inputStream.close();
            outputStream.flush();
            outputStream.close();
            file.setLastModified(timestamp);
        }

        Set<String> unnecessaryJars = new HashSet<String>(localJarsToTimestamps.keySet());
        unnecessaryJars.removeAll(remoteJarsToTimestamps.keySet());
        for (String jar : unnecessaryJars) {
            new File(cacheDir, jar).delete();
        }
        List<URL> urlsToLocalJars = new LinkedList<URL>();
        for (String jar : remoteJarsToTimestamps.keySet()) {
            urlsToLocalJars.add(new File(cacheDir, jar).toURI().toURL());
        }

        final URLClassLoader classLoader = new URLClassLoader(urlsToLocalJars.toArray(new URL[urlsToLocalJars.size()]));

        //all-permissions in the webstart jnlp file only applies to this class loader
        //the following code set the 'all' permission on the urlclassloader
        Policy.setPolicy(new Policy() {
            public PermissionCollection getPermissions(CodeSource codesource) {
                Permissions perms = new Permissions();
                perms.add(new AllPermission());
                return perms;
            }

            public void refresh() {
            }
        });

        Long maxTimestamp = new TreeSet<Long>(remoteJarsToTimestamps.values()).last();
        System.setProperty("starling.codeversion.timestamp", maxTimestamp.toString());

        try {
            Thread.currentThread().setContextClassLoader(classLoader);
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    UIManager.put("ClassLoader", classLoader); //for the look and feel
                    //trident uses the context classloader on the swing thread so we set it here
//          Thread.currentThread().setContextClassLoader(classLoader);
                }
            });
            Class launcher = classLoader.loadClass(mainClass);
            Method main = launcher.getMethod("main", new Class[]{String[].class});
            main.invoke(null, new Object[]{applicationArguments});
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static long copy(InputStream input, OutputStream output) throws IOException {
        byte[] buffer = new byte[1024];
        long count = 0;
        int n;
        while (-1 != (n = input.read(buffer))) {
            output.write(buffer, 0, n);
            count += n;
        }
        return count;
    }
}

class TeeOutputStream extends OutputStream {

    private OutputStream a;
    private OutputStream b;

    public TeeOutputStream(OutputStream a, OutputStream b) {
        this.a = a;
        this.b = b;
    }

    public void write(int c) throws IOException {
        a.write(c);
        b.write(c);
    }

    public void flush() throws IOException {
        a.flush();
        b.flush();
    }
}